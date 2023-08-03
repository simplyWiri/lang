#include "lexer.h"
#include "util.h"
#include <absl/container/inlined_vector.h>
#include <absl/types/span.h>
#include <absl/container/flat_hash_map.h>
#include <cstdint>
#include <charconv>
#include <optional>
#include <iostream>


namespace Ast
{

enum class NodeKind : uint16_t {
    ErrorNode,
    IntegerLiteral,
    VariableReference,
    BinaryExpression,
    ReturnExpression,
    File,
};

// Dynamically sized type (DST), must only be used as a pointer, the "end" of the struct is where the list of children_
// begins, child access should be done through `getChild(...)`.
struct GreenNode {
    using ChildType = util::t_ptr<GreenNode, Lexer::Token>;

protected:
    // The kind of syntax node
    NodeKind kind_;
    // The number of children that the node has
    std::uint16_t numChildren_;
    // The (textual) length of the node and its children
    std::uint32_t length_;
    // The start of an array of children, length of this array is `numChildren_`, this array is on the "stack" from the
    // context of the green node - but due to being a DST - it must only exist on the heap
    ChildType children_;

    ChildType populateChildren(absl::Span<const ChildType> children) {
        std::copy(children.begin(), children.end(), &children_);
        return children_;
    }

    explicit GreenNode(NodeKind kind, std::uint32_t length, absl::Span<const ChildType> children)
            : kind_(kind)
            , numChildren_(children.size())
            , length_(length)
            , children_(populateChildren(children)) { }

public:
    constexpr static bool Matches(NodeKind) {
        return true;
    }

    NodeKind kind() const {
        return kind_;
    }

    std::uint16_t numChildren() const {
        return numChildren_;
    }

    std::uint32_t length() const {
        return length_;
    }

    const ChildType& getChild(std::size_t index) const {
        assert(index < numChildren_);
        return (&children_)[index];
    }

    template<typename NodeType>
    const NodeType* as() const {
        assert(NodeType::Matches(kind_));
        return static_cast<const NodeType*>(this);
    }

    template<typename NodeType>
    const NodeType* nodeMatching(int ithOccurrence = 0) const {
        int currentIdx = 0;
        for (int i = 0; i < numChildren_; i++) {
            const auto& child = getChild(i);
            if (const auto* node = child.get<GreenNode>()) {
                if (NodeType::Matches(node->kind_)) {
                    if (currentIdx++ == ithOccurrence) {
                        return node->as<NodeType>();
                    }
                }
            }
        }
        return nullptr;
    }

    const Lexer::Token* tokenMatching(const absl::InlinedVector<Lexer::SyntaxKind, 16>& tokenKinds, int ithOccurrence = 0) const {
        int currentIdx = 0;
        for (int i = 0; i < numChildren_; i++) {
            const auto& child = getChild(i);
            if (const auto* token = child.get<Lexer::Token>()) {
                const auto iter = std::find(tokenKinds.begin(), tokenKinds.end(), token->kind);
                if (iter != tokenKinds.end()) {
                    if (currentIdx++ == ithOccurrence) {
                        return token;
                    }
                }
            }
        }
        return nullptr;
    }

    // Returned object's lifetime is bound to the arena.
    static GreenNode* create(util::arena& arena, NodeKind kind, std::uint32_t length, absl::Span<const ChildType> children = {}) {
        constexpr auto STRUCT_HEADER_SIZE = sizeof(GreenNode) - sizeof(void*);
        const auto CHILDREN_SIZE = sizeof(void*) * children.size();

        auto* memory = arena.alloc(STRUCT_HEADER_SIZE + CHILDREN_SIZE);
        return new (memory) GreenNode(kind, length, children);
    }

    // no move no copy, the type should only be used as a pointer.
    GreenNode& operator=(GreenNode&) = delete;
    GreenNode(GreenNode&) = delete;
    GreenNode& operator=(GreenNode&&) = delete;
    GreenNode(GreenNode&&) = delete;
};

namespace Green {

struct IntegerLiteralNode : public GreenNode {
    constexpr static bool Matches(NodeKind kind) {
        return kind == NodeKind::IntegerLiteral;
    }

    // it's not currently possible to ever be null, but illustrates the point of not being able to rely on the green
    // tree to be valid, or well-formed.
    std::optional<int> getInteger() const {
        if (const auto* maybeToken = tokenMatching( {Lexer::SyntaxKind::IntegerLiteral })) {
            int value;
            std::from_chars(maybeToken->text.begin(), maybeToken->text.end(), value);
            return value;
        }
        return std::nullopt;
    }
};

struct VariableReferenceNode : public GreenNode {
    constexpr static bool Matches(NodeKind kind) {
        return kind == NodeKind::VariableReference;
    }

    // it's not currently possible to ever be null, but illustrates the point of not being able to rely on the green
    // tree to be valid, or well-formed.
    std::optional<std::string_view> getVariableName() const {
        if (const auto* maybeToken = tokenMatching({Lexer::SyntaxKind::Identifier})) {
            return maybeToken->text;
        }
        return std::nullopt;
    }
};

struct BinaryExpressionNode : public GreenNode {
    constexpr static bool Matches(NodeKind kind) {
        return kind == NodeKind::BinaryExpression;
    }

    std::optional<const GreenNode*> getLeftExpression() const {
        return nodeMatching<GreenNode>(0);
    }

    std::optional<const Lexer::Token*> getOperator() const {
        return tokenMatching({Lexer::SyntaxKind::Plus, Lexer::SyntaxKind::Equals});
    }

    std::optional<const GreenNode*> getRightExpression() const {
        return nodeMatching<GreenNode>(1);
    }
};

struct ReturnExpressionNode : public GreenNode {
    constexpr static bool Matches(NodeKind kind) {
        return kind == NodeKind::ReturnExpression;
    }

    std::optional<const GreenNode*> getExpression() const {
        return nodeMatching<GreenNode>(0);
    }

    std::optional<const Lexer::Token*> getKeyword() const {
        return tokenMatching({Lexer::SyntaxKind::ReturnKeyword});
    }

    std::optional<const Lexer::Token*> getSemicolon() const {
        return tokenMatching({Lexer::SyntaxKind::Semicolon});
    }
};

}

class Parser {
    // intentionally 64 bytes
    struct GreenBuilder {
        explicit GreenBuilder(uint32_t id)
            : id(id), children({}) { }

        // The id of the green node, this is unique per-parser
        uint32_t id;
        // The children of the green node we are building
        absl::InlinedVector<GreenNode::ChildType, 6> children;
    };

    Lexer::SyntaxKind currentToken() const {
        return tokenState_.tokens_[tokenIndex_].kind;
    }
    // There is an expectation that at(Lexer::Whitespace) == false. I.e. the parser, while keeping whitespace - no
    // parsing code needs to know about its existence.
    bool at(Lexer::SyntaxKind kind) const {
        return currentToken() == kind;
    }

    void advance() {
        const auto& tokens = tokenState_.tokens_;

        do {
            auto* curToken = arena_.alloc<Lexer::Token>(tokens[tokenIndex_]);
            eventStack_.back().children.emplace_back(curToken);
            ++tokenIndex_;
        } while (at(Lexer::SyntaxKind::Whitespace));
    }

    // Returns an event index.
    uint32_t start() {
        const auto id = eventStack_.emplace_back(eventIndex_++).id;
        if (at(Lexer::SyntaxKind::Whitespace)) {
            advance();
        }
        return id;
    }

    GreenNode* finish(uint32_t id, NodeKind kind) {
        // Sums the length for each child, to get the total length for the parent.
        const auto getLength = [](const GreenBuilder& builder) -> uint32_t {
            uint32_t len = 0;
            for(const auto& child : builder.children) {
                if (child.holds<Lexer::Token>()) {
                    len += child.get<Lexer::Token>()->text.size();
                } else {
                    len += child.get<GreenNode>()->length();
                }
            }
            return len;
        };

        // The children which are going to be added to the current node
        const auto& builder = eventStack_.back();
        if (builder.id != id) {
            std::cerr << "Internal parser failure detected, unbalanced `eventStack_`." << std::endl;
            std::terminate();
        }

        // The length of the node in characters in the source code
        const auto nodeLength = getLength(builder);

        auto* node = GreenNode::create(arena_, kind, nodeLength, builder.children);
        eventStack_.pop_back();
        return node;
    }

    void appendChild(uint32_t id, GreenNode* node) {
        auto& builder = eventStack_.back();
        if (builder.id != id) {
            std::cerr << "Internal parser failure detected, unbalanced `eventStack_`." << std::endl;
            std::terminate();
        }
        builder.children.emplace_back(node);
    }
public:
    explicit Parser(const Lexer::TokenState& tokenState)
        : tokenState_(tokenState) { }

    // file = statement* EOF
    GreenNode* file() {
        const auto id = start();

        // do not consume EOF token.
        while (!at(Lexer::SyntaxKind::Eof)) {
            if (auto* lit = statement()) {
                appendChild(id, lit);
            }
        }

        return finish(id, NodeKind::File);
    }

    // statement = return_statement | expression
    GreenNode* statement() {
        // "return" expression ';'
        if (at(Lexer::SyntaxKind::ReturnKeyword)) {
            const auto id = start();
            advance(); // Consume "return" keyword

            // Consume an expression or emit an error
            if (auto* expr = expression()) {
                appendChild(id, expr);
            } else {
                appendChild(id, error());
            }

            if (at(Lexer::SyntaxKind::Semicolon)) {
                advance();
            }
            return finish(id, NodeKind::ReturnExpression);
        }
        return expression();
    }

    // expression = literal '+' expression
    //            | literal
    GreenNode* expression(Lexer::SyntaxKind left = Lexer::SyntaxKind::Eof) {
        auto* lhs = literal();

        while(true) {
            const auto right = currentToken();
            if (rightBindsTighter(left, right)) {
                const auto binaryExpression = start();
                advance();
                auto* rhs = expression(right);
                appendChild(binaryExpression, lhs);
                appendChild(binaryExpression, rhs);
                lhs = finish(binaryExpression, NodeKind::BinaryExpression);
            } else {
                break;
            }
        }

        return lhs;
    }

    static bool rightBindsTighter(Lexer::SyntaxKind left, Lexer::SyntaxKind right) {
        enum Side { Left, Right };

        auto getTightness = [](Lexer::SyntaxKind kind, Side side) -> std::optional<int> {
            // This may not be immediately clear how this functions. We map a syntax kind, which represents some kind of
            // binary expression, to a pair of 'Precedence' levels, i.e. how tightly a token should bind to other tokens
            // the reason we use a pair, is so we can specify left/right associative binding, beyond just precedence. If
            // the right value is less than the less, an operator is left associative, otherwise it is right associative
            static absl::flat_hash_map<Lexer::SyntaxKind, std::pair<int, int>> map = {
                    { Lexer::SyntaxKind::Equals, {1, 2} },
                    { Lexer::SyntaxKind::Plus, {4, 3} },
            };

            const auto maybeIter = map.find(kind);
            if (maybeIter != map.end()) {
                const auto [leftTightness, rightTightness] = maybeIter->second;

                return side == Left ? leftTightness : rightTightness;
            } else {
                return std::nullopt;
            }
        };

        auto rightTightness = getTightness(right, Side::Right);
        if (rightTightness == std::nullopt) return false;

        auto leftTightness = getTightness(left, Side::Left);
        if (leftTightness == std::nullopt) return true;

        return *rightTightness > *leftTightness;
    }

    // literal = integer_literal | variable_reference
    GreenNode* literal() {
        if (at(Lexer::SyntaxKind::IntegerLiteral)) {
            const auto id = start();
            advance();
            return finish(id, NodeKind::IntegerLiteral);
        } else if (at(Lexer::SyntaxKind::Identifier)) {
            const auto id = start();
            advance();
            return finish(id, NodeKind::VariableReference);
        }

        // end of a statement - not some kind of literal
        if (at(Lexer::SyntaxKind::Semicolon)) {
            return nullptr;
        } else {
            return error();
        }
    }

    // Anything can be an error.
    GreenNode* error() {
        const auto id = start();
        if (!at(Lexer::SyntaxKind::Eof)) {
            advance();
        }
        return finish(id, NodeKind::ErrorNode);
    }

    const Lexer::TokenState& tokenState_;
    util::arena arena_;

    std::size_t tokenIndex_{0};
    uint32_t eventIndex_{0};
    std::vector<GreenBuilder> eventStack_{};
};

}
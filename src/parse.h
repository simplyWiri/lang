#include "lexer.h"
#include "util.h"
#include <absl/container/inlined_vector.h>
#include <absl/types/span.h>
#include <cstdint>
#include <charconv>
#include <optional>
#include <iostream>


namespace Ast
{

enum class NodeKind : uint16_t {
    ErrorNode,
    IntegerLiteral,
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
    const NodeType* nodeMatching(int ithOccurrence = 0) const {
        int currentIdx = 0;
        for (int i = 0; i < numChildren_; i++) {
            const auto& child = getChild(i);
            if (const auto* node = child.get<GreenNode>()) {
                if (NodeType::Matches(node->kind_)) {
                    if (currentIdx++ == ithOccurrence) {
                        return static_cast<const NodeType*>(node);
                    }
                }
            }
        }
        return nullptr;
    }

    const Lexer::Token* tokenMatching(Lexer::SyntaxKind tokenKind, int ithOccurrence = 0) const {
        int currentIdx = 0;
        for (int i = 0; i < numChildren_; i++) {
            const auto& child = getChild(i);
            if (const auto* token = child.get<Lexer::Token>()) {
                if (token->is(tokenKind)) {
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
        if (const auto* maybeToken = tokenMatching(Lexer::SyntaxKind::IntegerLiteral)) {
            int value;
            std::from_chars(maybeToken->text.begin(), maybeToken->text.end(), value);
            return value;
        }
        return std::nullopt;
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

    // There is an expectation that at(Lexer::Whitespace) == false. I.e. the parser, while keeping whitespace - no
    // parsing code needs to know about its existence.
    bool at(Lexer::SyntaxKind kind) const {
        return tokenState_.tokens_[tokenIndex_].is(kind);
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

    // file = literal* EOF
    GreenNode* file() {
        const auto id = start();

        // do not consume EOF token.
        while (!at(Lexer::SyntaxKind::Eof)) {
            if (auto* lit = literal()) {
                appendChild(id, lit);
            } else {
                appendChild(id, error());
            }
        }

        return finish(id, NodeKind::File);
    }

    // literal = integer_literal
    GreenNode* literal() {
        if (at(Lexer::SyntaxKind::IntegerLiteral)) {
            const auto id = start();
            advance();
            return finish(id, NodeKind::IntegerLiteral);
        }
        return nullptr;
    }

    // Anything can be an error.
    GreenNode* error() {
        const auto id = start();
        advance();
        return finish(id, NodeKind::ErrorNode);
    }

    const Lexer::TokenState& tokenState_;
    util::arena arena_;

    std::size_t tokenIndex_{0};
    uint32_t eventIndex_{0};
    std::vector<GreenBuilder> eventStack_{};
};

}
#include "lexer.h"
#include "util.h"
#include <absl/types/span.h>
#include <cstdint>

namespace Ast
{

enum class NodeKind : uint16_t {
    ErrorNode
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

}
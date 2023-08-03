#include "doctest.h"
#include "parse.h"

TEST_SUITE_BEGIN("Parse Tests");

TEST_CASE("Green Node Basics") {
    // Keep the struct as small as possible. This struct could be pointer sized if children_ could ever be empty, by
    // using the implicit space after the struct as the array (as we do for children with an index > 0). However, the
    // current design ensures that every node will have at least a single child (the token which formed the grammar
    // construct), so this optimization is unnecessary.
    CHECK_EQ(sizeof(Ast::GreenNode), sizeof(void*) * 2);
    // type is allocated from arenas, must be trivially destructible
    CHECK(std::is_trivially_destructible_v<Ast::GreenNode>);
    // pointer only type, no copy no move
    CHECK(!std::is_copy_constructible_v<Ast::GreenNode>); // no copy
    CHECK(!std::is_move_constructible_v<Ast::GreenNode>); // no move
}

TEST_CASE("Green Node Construction") {
    using enum Ast::NodeKind;
    util::arena arena;

    SUBCASE("No Children") {
        auto* node = Ast::GreenNode::create(arena, ErrorNode, 1);
        CHECK_EQ(node->numChildren(), 0);
        CHECK_EQ(node->kind(), ErrorNode);
        CHECK_EQ(node->length(), 1);
    }

    Lexer::Token tok{Lexer::SyntaxKind::IntegerLiteral, "5"};
    Lexer::Token tok2{Lexer::SyntaxKind::IntegerLiteral, "10"};

    SUBCASE("One Child") {
        auto* node = Ast::GreenNode::create(arena, ErrorNode, 1, {&tok});
        CHECK_EQ(node->numChildren(), 1);
        CHECK_EQ(node->getChild(0).get<Lexer::Token>()->text, tok.text);
    }

    SUBCASE("Two Children") {
        // Repetition ensures we aren't stomping on any consecutive bits of memory
        auto* node = Ast::GreenNode::create(arena, ErrorNode, 1, {&tok, &tok2});
        auto* node2 = Ast::GreenNode::create(arena, ErrorNode, 1, {&tok, &tok2});
        auto* node3 = Ast::GreenNode::create(arena, ErrorNode, 1, {&tok, &tok2});

        CHECK(reinterpret_cast<std::byte*>(node) + 24 == reinterpret_cast<std::byte*>(node2));
        CHECK(reinterpret_cast<std::byte*>(node2) + 24 == reinterpret_cast<std::byte*>(node3));

        for (const auto* n : { node, node2, node3}) {
            CHECK_EQ(n->numChildren(), 2);
            CHECK_EQ(n->getChild(0).get<Lexer::Token>()->text, tok.text);
            CHECK_EQ(n->getChild(1).get<Lexer::Token>()->text, tok2.text);
        }
    }

    auto* childNode = Ast::GreenNode::create(arena, ErrorNode, 1, {&tok});
    SUBCASE("Green Node Child") {
        auto* node = Ast::GreenNode::create(arena, ErrorNode, 1, {&tok2, childNode});
        CHECK_EQ(node->numChildren(), 2);
        CHECK_EQ(node->getChild(0).get<Lexer::Token>()->text, tok2.text);
        CHECK_EQ(node->getChild(1).get<Ast::GreenNode>()->numChildren(), 1);
    }
}

TEST_CASE("Green Node Access") {
    using enum Ast::NodeKind;
    util::arena arena;

    Lexer::Token firstToken{Lexer::SyntaxKind::IntegerLiteral, "5"};
    Lexer::Token secndToken{Lexer::SyntaxKind::IntegerLiteral, "5"};
    auto* firstNode = Ast::GreenNode::create(arena, ErrorNode, 1, {&firstToken});
    auto* secndNode = Ast::GreenNode::create(arena, ErrorNode, 1, {&secndToken});

    const auto* packedNode = Ast::GreenNode::create(arena, ErrorNode, 4,
                                                    {&firstToken, &secndToken, firstNode, secndNode});
    CHECK_EQ(packedNode->numChildren(), 4);
    CHECK_EQ(packedNode->nodeMatching<Ast::GreenNode>(0), firstNode);
    CHECK_EQ(packedNode->nodeMatching<Ast::GreenNode>(1), secndNode);
    CHECK_EQ(packedNode->nodeMatching<Ast::GreenNode>(2), nullptr);

    CHECK_EQ(packedNode->tokenMatching(Lexer::SyntaxKind::IntegerLiteral, 0), &firstToken);
    CHECK_EQ(packedNode->tokenMatching(Lexer::SyntaxKind::IntegerLiteral, 1), &secndToken);
    CHECK_EQ(packedNode->tokenMatching(Lexer::SyntaxKind::IntegerLiteral, 2), nullptr);

    // Token types which don't exist simply return a nullptr, this is a soft interface to support erroneous trees.
    CHECK_EQ(packedNode->tokenMatching(Lexer::SyntaxKind::ErrorToken, 0), nullptr);
}

struct ParserFixture {
    const Ast::GreenNode* getRoot(const std::string& str) {
        auto& state = states_.emplace_back(Lexer::lex(str));
        auto& parser = parsers_.emplace_back(state);
        const auto* file = parser.file();
        checkPostConditions(file, str);
        return file;
    }

    template<typename NodeT>
    static const NodeT* checkIs(const Ast::GreenNode* node) {
        REQUIRE(node);
        CHECK(NodeT::Matches(node->kind()));
        return node->as<NodeT>();
    }

    template<typename NodeT>
    static const NodeT* checkIs(const std::optional<const Ast::GreenNode*> maybeNode) {
        REQUIRE(maybeNode);
        return checkIs<NodeT>(*maybeNode);
    }

private:
    static void checkPostConditions(const Ast::GreenNode* syntaxTree, const std::string& grammarStr) {
        CHECK_EQ(syntaxTree->length(), grammarStr.length());
    }

    std::list<Lexer::TokenState> states_;
    std::list<Ast::Parser> parsers_;
};


TEST_CASE_FIXTURE(ParserFixture, "Baseline") {
    using namespace Ast::Green;

    SUBCASE("Simple") {
        // Creates the tree:
        // File
        //    IntegerLiteralNode
        //        Token(IntegerLiteral) (9)
        const auto* root = getRoot("9");
        CHECK_EQ(root->numChildren(), 1);
        CHECK_EQ(root->length(), 1);

        const auto* integerLiteralNode = root->nodeMatching<IntegerLiteralNode>();
        CHECK_EQ(integerLiteralNode->kind(), Ast::NodeKind::IntegerLiteral);
        CHECK_EQ(integerLiteralNode->numChildren(), 1);
        CHECK_EQ(integerLiteralNode->length(), 1);
        CHECK_EQ(integerLiteralNode->getInteger(), 9);

        // Ensure the token is a child of its grammar production
        CHECK(integerLiteralNode->tokenMatching(Lexer::SyntaxKind::IntegerLiteral));
    }
    // Leading whitespace should not cause issues with grammar productions.
    SUBCASE("Leading Whitespace") {
        const auto* root = getRoot("   9   ");

        CHECK_EQ(root->kind(), Ast::NodeKind::File);
        CHECK_EQ(root->numChildren(), 2); // white space token, integer literal of '9'
        CHECK_EQ(root->length(), 7);

        CHECK(root->tokenMatching(Lexer::SyntaxKind::Whitespace));
        CHECK(root->nodeMatching<IntegerLiteralNode>());

        const auto* integerLiteralNode = root->nodeMatching<IntegerLiteralNode>();
        CHECK_EQ(integerLiteralNode->numChildren(), 2);
        CHECK_EQ(integerLiteralNode->length(), 4);
        CHECK_EQ(integerLiteralNode->getInteger(), 9);

        CHECK(integerLiteralNode->tokenMatching(Lexer::SyntaxKind::IntegerLiteral));
    }
    // Erroneous tokens should not result in a complete failure to get output
    SUBCASE("Error Token") {
        Lexer::TokenState state = Lexer::lex("^ 8");
        Ast::Parser parser{state};

        const auto* root = parser.file();
        CHECK_EQ(root->numChildren(), 2); // error, integer literal of '8'

        const auto* integerLiteralNode = root->nodeMatching<IntegerLiteralNode>();
        CHECK_EQ(integerLiteralNode->getInteger(), 8);
    }
}

TEST_CASE_FIXTURE(ParserFixture, "Binary Expressions") {
    using namespace Ast::Green;

    SUBCASE("Basic") {
        const auto* root = getRoot("9 + 5");
        const auto* binaryExpression = root->nodeMatching<BinaryExpressionNode>();

        const auto* lhs = checkIs<IntegerLiteralNode>(binaryExpression->getLeftExpression());
        CHECK_EQ(lhs->getInteger(), 9);

        const auto* rhs = checkIs<IntegerLiteralNode>(binaryExpression->getRightExpression());
        CHECK_EQ(rhs->getInteger(), 5);
    }

    SUBCASE("Left Associativity") {
        const auto* root = getRoot("9 + 5 + 4");
        const auto* binaryExpression = root->nodeMatching<BinaryExpressionNode>();

        const auto* lhs = checkIs<IntegerLiteralNode>(binaryExpression->getLeftExpression());
        CHECK_EQ(lhs->getInteger(), 9);

        const auto* rhs = checkIs<BinaryExpressionNode>(binaryExpression->getRightExpression());

        const auto* nestedLeft = checkIs<IntegerLiteralNode>(rhs->getLeftExpression());
        CHECK_EQ(nestedLeft->getInteger(), 5);

        const auto* nestedRight = checkIs<IntegerLiteralNode>(rhs->getRightExpression());
        CHECK_EQ(nestedRight->getInteger(), 4);
    }

    SUBCASE("Missing LHS") {
        const auto* root = getRoot(" + 5");

        const auto* integerLiteral = root->nodeMatching<IntegerLiteralNode>();
        CHECK_EQ(integerLiteral->getInteger(), 5);
    }

    SUBCASE("Missing RHS") {
        const auto* root = getRoot("9 + ");
        const auto* binaryExpression = root->nodeMatching<BinaryExpressionNode>();

        const auto* lhs = checkIs<IntegerLiteralNode>(binaryExpression->getLeftExpression());
        CHECK_EQ(lhs->getInteger(), 9);

        auto rhs = binaryExpression->getRightExpression();
        REQUIRE(rhs);
        CHECK_EQ((*rhs)->kind(), Ast::NodeKind::ErrorNode);
    }
}

TEST_CASE_FIXTURE(ParserFixture, "Variable References") {
    using namespace Ast::Green;

    SUBCASE("Binary Expression") {
        const auto* root = getRoot("i + ii");

        const auto* binaryExpression = root->nodeMatching<BinaryExpressionNode>();

        const auto* lhs = checkIs<VariableReferenceNode>(binaryExpression->getLeftExpression());
        CHECK_EQ(lhs->getVariableName(), "i");

        const auto* rhs = checkIs<VariableReferenceNode>(binaryExpression->getRightExpression());
        CHECK_EQ(rhs->getVariableName(), "ii");
    }
    SUBCASE("Literal") {
        const auto* root = getRoot("iii");

        const auto* variableReference = root->nodeMatching<VariableReferenceNode>();
        CHECK_EQ(variableReference->getVariableName(), "iii");
    }
}

TEST_CASE_FIXTURE(ParserFixture, "Return Expression") {
    using namespace Ast::Green;

    SUBCASE("Return Literal") {
        const auto* root = getRoot("return 4;");

        const auto* returnExpression = root->nodeMatching<ReturnExpressionNode>();
        CHECK(returnExpression->getKeyword());
        CHECK(returnExpression->getSemicolon());

        const auto* literal = checkIs<IntegerLiteralNode>(returnExpression->getExpression());
        CHECK_EQ(literal->getInteger(), 4);
    }
    SUBCASE("Return Expression") {
        const auto* root = getRoot("return 6 + i;");

        const auto* returnExpression = root->nodeMatching<ReturnExpressionNode>();
        CHECK(returnExpression->getKeyword());
        CHECK(returnExpression->getSemicolon());

        const auto* binaryExpression = checkIs<BinaryExpressionNode>(returnExpression->getExpression());

        const auto* lhs = checkIs<IntegerLiteralNode>(binaryExpression->getLeftExpression());
        CHECK_EQ(lhs->getInteger(), 6);

        const auto* rhs = checkIs<VariableReferenceNode>(binaryExpression->getRightExpression());
        CHECK_EQ(rhs->getVariableName(), "i");
    }

    SUBCASE("Missing Semicolon") {
        const auto* root = getRoot("return 9");

        const auto* returnExpression = root->nodeMatching<ReturnExpressionNode>();
        CHECK(returnExpression->getKeyword());
        CHECK_EQ(returnExpression->getSemicolon(), nullptr);

        const auto* lhs = checkIs<IntegerLiteralNode>(returnExpression->getExpression());
        CHECK_EQ(lhs->getInteger(), 9);
    }

    SUBCASE("Missing Expression") {
        const auto* root = getRoot("return ;");

        const auto* returnExpression = root->nodeMatching<ReturnExpressionNode>();
        CHECK(returnExpression->getKeyword());
        CHECK(returnExpression->getSemicolon());
        CHECK_EQ(returnExpression->getExpression().value()->kind(), Ast::NodeKind::ErrorNode);
    }
}

TEST_SUITE_END;
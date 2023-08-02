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

TEST_CASE("Parse File") {
    Lexer::TokenState state = Lexer::lex("9");
    Ast::Parser parser{state};

    const auto* root = parser.file();
    CHECK_EQ(root->kind(), Ast::NodeKind::File);
    CHECK_EQ(root->numChildren(), 1);
    CHECK_EQ(root->length(), 1);

    CHECK(root->nodeMatching<Ast::Green::IntegerLiteralNode>());

    const auto* integerLiteralNode = root->nodeMatching<Ast::Green::IntegerLiteralNode>();
    CHECK_EQ(integerLiteralNode->kind(), Ast::NodeKind::IntegerLiteral);
    CHECK_EQ(integerLiteralNode->numChildren(), 1);
    CHECK_EQ(integerLiteralNode->length(), 1);
    CHECK_EQ(integerLiteralNode->getInteger(), 9);

    CHECK(integerLiteralNode->tokenMatching(Lexer::SyntaxKind::IntegerLiteral));
}

TEST_CASE("Parse File with Whitespace") {
    Lexer::TokenState state = Lexer::lex("   9   ");
    Ast::Parser parser{state};

    const auto* root = parser.file();
    CHECK_EQ(root->kind(), Ast::NodeKind::File);
    CHECK_EQ(root->numChildren(), 2); // error, integer literal of '9'
    CHECK_EQ(root->length(), 7);

    CHECK(root->nodeMatching<Ast::Green::IntegerLiteralNode>());

    const auto* integerLiteralNode = root->nodeMatching<Ast::Green::IntegerLiteralNode>();
    CHECK_EQ(integerLiteralNode->kind(), Ast::NodeKind::IntegerLiteral);
    CHECK_EQ(integerLiteralNode->numChildren(), 2);
    CHECK_EQ(integerLiteralNode->length(), 4);
    CHECK_EQ(integerLiteralNode->getInteger(), 9);

    CHECK(integerLiteralNode->tokenMatching(Lexer::SyntaxKind::IntegerLiteral));
}

TEST_CASE("Erroneous Token") {
    Lexer::TokenState state = Lexer::lex("^ 8");
    Ast::Parser parser{state};

    const auto* root = parser.file();
    CHECK_EQ(root->kind(), Ast::NodeKind::File);
    CHECK_EQ(root->numChildren(), 2); // error, integer literal of '8'
    CHECK_EQ(root->length(), 3);

    CHECK(root->nodeMatching<Ast::Green::IntegerLiteralNode>());

    const auto* integerLiteralNode = root->nodeMatching<Ast::Green::IntegerLiteralNode>();
    CHECK_EQ(integerLiteralNode->kind(), Ast::NodeKind::IntegerLiteral);
    CHECK_EQ(integerLiteralNode->numChildren(), 1);
    CHECK_EQ(integerLiteralNode->length(), 1);
    CHECK_EQ(integerLiteralNode->getInteger(), 8);

    CHECK(integerLiteralNode->tokenMatching(Lexer::SyntaxKind::IntegerLiteral));
}

TEST_CASE("Binary Expression") {
    auto checkIntegerNode = [](const Ast::GreenNode* node, int numChildren, int length, int literalValue) {
        const auto* integerLiteral = node->as<Ast::Green::IntegerLiteralNode>();
        CHECK(integerLiteral);
        CHECK_EQ(integerLiteral->numChildren(), numChildren);
        CHECK_EQ(integerLiteral->length(), length);
        CHECK_EQ(integerLiteral->getInteger(), literalValue);
    };

    SUBCASE("Basic") {
        Lexer::TokenState state = Lexer::lex("9 + 5");
        Ast::Parser parser{state};

        const auto* root = parser.file();
        CHECK_EQ(root->numChildren(), 1);
        CHECK_EQ(root->length(), 5);

        CHECK(root->nodeMatching<Ast::Green::BinaryExpressionNode>());

        const auto* binaryExpression = root->nodeMatching<Ast::Green::BinaryExpressionNode>();
        CHECK_EQ(binaryExpression->numChildren(), 4);
        CHECK_EQ(binaryExpression->length(), 5);

        auto lhs = binaryExpression->getLeftExpression();
        checkIntegerNode(*lhs, 2, 2, 9);

        auto rhs = binaryExpression->getRightExpression();
        checkIntegerNode(*rhs, 1, 1, 5);
    }

    SUBCASE("Left Associativity") {
        Lexer::TokenState state = Lexer::lex("9 + 5 + 4");
        Ast::Parser parser{state};

        const auto* root = parser.file();
        CHECK_EQ(root->numChildren(), 1);
        CHECK_EQ(root->length(), 9);

        CHECK(root->nodeMatching<Ast::Green::BinaryExpressionNode>());

        const auto* binaryExpression = root->nodeMatching<Ast::Green::BinaryExpressionNode>();
        CHECK_EQ(binaryExpression->kind(), Ast::NodeKind::BinaryExpression);
        CHECK_EQ(binaryExpression->numChildren(), 4);
        CHECK_EQ(binaryExpression->length(), 9);

        auto lhs = binaryExpression->getLeftExpression();
        checkIntegerNode(*lhs, 2, 2, 9);

        auto rhs = binaryExpression->getRightExpression();
        CHECK(rhs);
        const auto* rightExpression = (*rhs)->as<Ast::Green::BinaryExpressionNode>();
        CHECK(rightExpression);
        CHECK_EQ(rightExpression->numChildren(), 4);
        CHECK_EQ(rightExpression->length(), 5);

        auto nestedLhs = rightExpression->getLeftExpression();
        checkIntegerNode(*nestedLhs, 2, 2, 5);

        auto nestedRhs = rightExpression->getRightExpression();
        checkIntegerNode(*nestedRhs, 1, 1, 4);
    }

    SUBCASE("Missing LHS") {
        Lexer::TokenState state = Lexer::lex(" + 5");
        Ast::Parser parser{state};

        const auto* root = parser.file();
        CHECK_EQ(root->numChildren(), 3);
        CHECK_EQ(root->length(), 4);

        const auto* integerLiteral = root->nodeMatching<Ast::Green::IntegerLiteralNode>();
        CHECK_EQ(integerLiteral->getInteger(), 5);
    }

    SUBCASE("Missing RHS") {
        Lexer::TokenState state = Lexer::lex("9 + ");
        Ast::Parser parser{state};
        const auto* root = parser.file();
        CHECK_EQ(root->numChildren(), 1);
        CHECK_EQ(root->length(), 4);

        CHECK(root->nodeMatching<Ast::Green::BinaryExpressionNode>());

        const auto* binaryExpression = root->nodeMatching<Ast::Green::BinaryExpressionNode>();
        CHECK_EQ(binaryExpression->numChildren(), 4);
        CHECK_EQ(binaryExpression->length(), 4);

        auto lhs = binaryExpression->getLeftExpression();
        checkIntegerNode(*lhs, 2, 2, 9);

        const auto* rhs = *binaryExpression->getRightExpression();
        CHECK_EQ(rhs->kind(), Ast::NodeKind::ErrorNode);
    }
}

TEST_CASE("Variable Reference") {
    SUBCASE("Binary Expression") {
        Lexer::TokenState state = Lexer::lex("i + ii");
        Ast::Parser parser{ state };

        const auto* root = parser.file();
        const auto* binaryExpression = root->nodeMatching<Ast::Green::BinaryExpressionNode>();

        auto lhs = (*binaryExpression->getLeftExpression())->as<Ast::Green::VariableReferenceNode>();
        CHECK_EQ(lhs->getVariableName(), "i");

        auto rhs = (*binaryExpression->getRightExpression())->as<Ast::Green::VariableReferenceNode>();
        CHECK_EQ(rhs->getVariableName(), "ii");
    }
    SUBCASE("Literal") {
        Lexer::TokenState state = Lexer::lex("iii");
        Ast::Parser parser{ state };

        const auto* root = parser.file();

        const auto* variableReference = root->nodeMatching<Ast::Green::VariableReferenceNode>();
        CHECK_EQ(variableReference->getVariableName(), "iii");
    }
}

TEST_SUITE_END;
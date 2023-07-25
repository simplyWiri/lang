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
    Lexer::TokenState state = Lexer::lex("i 8");
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

TEST_SUITE_END;
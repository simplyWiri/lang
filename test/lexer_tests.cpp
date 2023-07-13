#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "lexer.h"

void checkPostConditions(const Lexer::TokenState& tokenState, const std::string& basis) {
    // The input string is faithfully stored in a point of truth in the token state
    CHECK(tokenState.file_->text_ == basis);

    if (tokenState.tokens_.empty()) {
        return;
    }

    // The concatenation of all tokens (and their text) is equal to the input string.
    const auto& tokens = tokenState.tokens_;
    std::size_t tokenIdx = 0, tokenTextIdx = 0;

    for (const auto chr : basis) {
        CHECK(tokenIdx < tokens.size());

        const auto& token = tokens[tokenIdx];
        CHECK_EQ(chr, token.text[tokenTextIdx++]);

        // When we are at the end of a text sub-string, reset the text counter - move onto the next token
        if (tokenTextIdx == token.text.size()) {
            ++tokenIdx;
            tokenTextIdx = 0;
        }
    }

    CHECK(tokens.size() > tokenIdx);
    CHECK(tokens[tokenIdx].is(Lexer::SyntaxKind::Eof));
}

TEST_SUITE_BEGIN("Lexer Tests");

void checkToken(const Lexer::TokenState& lexState, int idx, Lexer::SyntaxKind kind, std::string_view text) {
    CHECK(lexState.tokens_[idx].is(kind));
    CHECK_EQ(lexState.tokens_[idx].text, text);
}

TEST_CASE("Integer Literals") {
    const std::string exampleString = "1 23 456 7890";

    const auto lexState = Lexer::lex(exampleString);

    // Check that the tokens produced are correct
    CHECK(lexState.tokens_.size() == 8);

    checkToken(lexState, 0, Lexer::SyntaxKind::IntegerLiteral, "1");
    checkToken(lexState, 1, Lexer::SyntaxKind::Whitespace, " ");
    checkToken(lexState, 2, Lexer::SyntaxKind::IntegerLiteral, "23");
    checkToken(lexState, 3, Lexer::SyntaxKind::Whitespace, " ");
    checkToken(lexState, 4, Lexer::SyntaxKind::IntegerLiteral, "456");
    checkToken(lexState, 5, Lexer::SyntaxKind::Whitespace, " ");
    checkToken(lexState, 6, Lexer::SyntaxKind::IntegerLiteral, "7890");
    checkToken(lexState, 7, Lexer::SyntaxKind::Eof, "");

    // Check that the base state is correct
    checkPostConditions(lexState, exampleString);
}

TEST_CASE("Symbols") {
    const std::string str = "= ==";
    const auto lexState = Lexer::lex(str);

    CHECK(lexState.tokens_[0].is(Lexer::SyntaxKind::Equals));
    CHECK(lexState.tokens_[1].is(Lexer::SyntaxKind::Whitespace));
    // Check that `==` does not bind to a singe token.
    CHECK(lexState.tokens_[2].is(Lexer::SyntaxKind::Equals));
    CHECK(lexState.tokens_[3].is(Lexer::SyntaxKind::Equals));
    CHECK(lexState.tokens_[4].is(Lexer::SyntaxKind::Eof));

    checkPostConditions(lexState, str);
}

TEST_CASE("Identifiers") {
    const std::string str = "i iii abcdefghijk i123 3i4";
    const auto lexState = Lexer::lex(str);

    CHECK_EQ(lexState.tokens_.size(), 11);

    checkToken(lexState, 0, Lexer::SyntaxKind::Identifier, "i");
    checkToken(lexState, 2, Lexer::SyntaxKind::Identifier, "iii");
    checkToken(lexState, 4, Lexer::SyntaxKind::Identifier, "abcdefghijk");
    checkToken(lexState, 6, Lexer::SyntaxKind::Identifier, "i123");
    checkToken(lexState, 8, Lexer::SyntaxKind::IntegerLiteral, "3");
    checkToken(lexState, 9, Lexer::SyntaxKind::Identifier, "i4");

    CHECK(lexState.tokens_[10].is(Lexer::SyntaxKind::Eof));

    checkPostConditions(lexState, str);

    SUBCASE("Identifier Assignment") {
        const std::string example = "i = 55";
        const auto state = Lexer::lex(example);

        checkToken(state, 0, Lexer::SyntaxKind::Identifier, "i");
        checkToken(state, 2, Lexer::SyntaxKind::Equals, "=");
        checkToken(state, 4, Lexer::SyntaxKind::IntegerLiteral, "55");

        checkPostConditions(state, example);
    }
}

// All but the last token (EOF) should be unknown, however when concatenating the tokens we must be able to restore the
// full input.
TEST_CASE("Unknown Symbols") {
    const std::string exampleString = "!@#$%^&*()";
    const auto lexState = Lexer::lex(exampleString);

    const auto& tokens = lexState.tokens_;
    for (std::size_t i = 0; i < tokens.size() - 1; i++) {
        CHECK(tokens[i].is(Lexer::SyntaxKind::ErrorToken));
    }
    CHECK(tokens[tokens.size() - 1].is(Lexer::SyntaxKind::Eof));

    checkPostConditions(lexState, exampleString);
}

TEST_SUITE_END;
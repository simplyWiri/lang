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

TEST_CASE("Integer Literals") {
    const std::string exampleString = "1 23 456 7890";

    const auto lexState = Lexer::lex(exampleString);

    // Check that the tokens produced are correct
    CHECK(lexState.tokens_.size() == 8);

    auto checkIndex = [&](const int idx, Lexer::SyntaxKind kind, std::string_view text) -> void {
        CHECK(lexState.tokens_[idx].is(kind));
        CHECK_EQ(lexState.tokens_[idx].text, text);
    };

    checkIndex(0, Lexer::SyntaxKind::IntegerLiteral, "1");
    checkIndex(1, Lexer::SyntaxKind::Whitespace, " ");
    checkIndex(2, Lexer::SyntaxKind::IntegerLiteral, "23");
    checkIndex(3, Lexer::SyntaxKind::Whitespace, " ");
    checkIndex(4, Lexer::SyntaxKind::IntegerLiteral, "456");
    checkIndex(5, Lexer::SyntaxKind::Whitespace, " ");
    checkIndex(6, Lexer::SyntaxKind::IntegerLiteral, "7890");
    checkIndex(7, Lexer::SyntaxKind::Eof, "");

    // Check that the base state is correct
    checkPostConditions(lexState, exampleString);
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

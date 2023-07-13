#pragma once

#include <ostream>
#include <utility>
#include <vector>
#include <memory>

namespace Lexer {

enum class SyntaxKind : uint16_t {
    ErrorToken,
    Whitespace,
    IntegerLiteral,
    Eof
};

inline std::ostream& operator <<(std::ostream& os, SyntaxKind kind) {
    switch (kind) {
        case SyntaxKind::ErrorToken: return os << "ErrorToken";
        case SyntaxKind::Whitespace: return os << "Whitespace";
        case SyntaxKind::IntegerLiteral: return os << "IntegerLiteral";
        case SyntaxKind::Eof: return os << "Eof";
    }
}

// This enables the use of using `string_view`'s to the data stored by the file
struct File {
    std::string text_;
    explicit File(std::string text)
        : text_(std::move(text)) { }
};

struct Token {
    SyntaxKind kind;
    std::string_view text;

    Token(SyntaxKind k, std::string_view t)
        : kind(k), text(t) { }


    template<typename... Kinds>
    bool is(Kinds... kinds) const {
        static_assert((std::is_same_v<decltype(kinds), SyntaxKind> && ...), "Expected `Kinds`... to be a SyntaxKind");

        return ((kind == kinds) || ...);
    }
};

struct TokenState {
    std::shared_ptr<File> file_;
    std::vector<Token> tokens_;
};

inline TokenState lex(const std::string& str) {
    TokenState state = { std::make_shared<File>(str), {} };
    const auto& text = state.file_->text_;
    auto& tokens = state.tokens_;

    for (auto it = text.begin(); it != text.end();) {
        const auto sequenceBegin = it;
        const auto chr = *it;

        if (isspace(chr)) {
            while (isspace(*it) && it != text.end()) {
                ++it;
            }

            tokens.emplace_back(SyntaxKind::Whitespace, std::string_view{sequenceBegin, it});
        } else if (std::isdigit(chr)) {
            while (std::isdigit(*it) && it != text.end()) {
                ++it;
            }

            tokens.emplace_back(SyntaxKind::IntegerLiteral, std::string_view{sequenceBegin, it});
        } else {
            ++it;
            tokens.emplace_back(SyntaxKind::ErrorToken, std::string_view{sequenceBegin, it});
        }
    }

    tokens.emplace_back(SyntaxKind::Eof, std::string_view{});

    return state;
}
}
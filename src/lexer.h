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
    Identifier,
    Equals,
    Semicolon,
    ReturnKeyword,
    Eof
};

inline std::ostream& operator <<(std::ostream& os, SyntaxKind kind) {
    switch (kind) {
        using enum SyntaxKind;
        case ErrorToken: return os << "ErrorToken";
        case Whitespace: return os << "Whitespace";
        case IntegerLiteral: return os << "IntegerLiteral";
        case Identifier: return os << "Identifier";
        case Equals: return os << "Equals";
        case Semicolon: return os << "Semicolon";
        case ReturnKeyword: return os << "ReturnKeyword";
        case Eof: return os << "Eof";
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

struct Tokenizer {
    using StringIterator = std::string::const_iterator;

    explicit Tokenizer(TokenState& tokenState)
        : tokenState_(tokenState)
        , it_(tokenState.file_->text_.begin())
        , endIt_(tokenState.file_->text_.end())
        , sequenceBeginIt_(tokenState.file_->text_.begin()) { }

    std::string_view getCurrentSlice() const {
        return { sequenceBeginIt_, it_ };
    }

    void addToken(SyntaxKind kind) {
        tokenState_.tokens_.emplace_back(kind, getCurrentSlice());
    }

    char cur() const {
        return *it_;
    }

    void lex() {
        using enum SyntaxKind;

        while(it_ != endIt_) {
            const auto chr = cur();

            if (isspace(chr)) {
                while (isspace(cur()) && it_ != endIt_) ++it_;

                addToken(Whitespace);
            } else if (isdigit(chr)) {
                while (isdigit(cur()) && it_ != endIt_) ++it_;

                addToken(IntegerLiteral);
            } else if (chr == '=') {
                ++it_;
                addToken(Equals);
            } else if (chr == ';') {
                ++it_;
                addToken(Semicolon);
            }else if (isalpha(chr)) {
                while (isalnum(cur()) && it_ != endIt_) ++it_;

                if (getCurrentSlice() == "return") {
                    addToken(ReturnKeyword);
                } else {
                    addToken(Identifier);
                }
            } else {
                // if we don't recognise this character, simply ingest it as an error token and continue.
                ++it_;
                addToken(ErrorToken);
            }

            sequenceBeginIt_ = it_;
        }

        addToken(Eof);
    }

    TokenState& tokenState_;
    StringIterator it_;
    StringIterator endIt_;
    StringIterator sequenceBeginIt_;
};

inline TokenState lex(const std::string& str) {
    TokenState state = { std::make_shared<File>(str), {} };

    Tokenizer tokenizer{state};
    tokenizer.lex();

    return state;
}
}
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
    Equals,
    Eof
};

inline std::ostream& operator <<(std::ostream& os, SyntaxKind kind) {
    switch (kind) {
        case SyntaxKind::ErrorToken: return os << "ErrorToken";
        case SyntaxKind::Whitespace: return os << "Whitespace";
        case SyntaxKind::IntegerLiteral: return os << "IntegerLiteral";
        case SyntaxKind::Equals: return os << "Equals";
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

struct Tokenizer {
    using StringIterator = std::string::const_iterator;

    explicit Tokenizer(TokenState& tokenState)
        : tokenState_(tokenState)
        , it_(tokenState.file_->text_.begin())
        , endIt_(tokenState.file_->text_.end())
        , sequenceBeginIt_(tokenState.file_->text_.begin()) { }

    void addToken(SyntaxKind kind) {
        tokenState_.tokens_.emplace_back(kind, std::string_view{sequenceBeginIt_, it_});
    }

    char cur() const {
        return *it_;
    }

    void lex() {
        while(it_ != endIt_) {
            const auto chr = cur();

            if (isspace(chr)) {
                while (isspace(cur()) && it_ != endIt_) ++it_;

                addToken(SyntaxKind::Whitespace);
            } else if (isdigit(chr)) {
                while (isdigit(cur()) && it_ != endIt_) ++it_;

                addToken(SyntaxKind::IntegerLiteral);
            } else if (chr == '=') {
                ++it_;
                addToken(SyntaxKind::Equals);
            } else {
                // if we don't recognise this character, simply ingest it as an error token and continue.
                ++it_;
                addToken(SyntaxKind::ErrorToken);
            }

            sequenceBeginIt_ = it_;
        }

        addToken(SyntaxKind::Eof);
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
#include <cstdint>
#include <type_traits>
#include <utility>
#include <memory>
#include <cassert>


namespace util {

// a very simple non-owning pointer which can take one of two values. Any attempt to access a type not in the template
// will result in a compilation error. Guaranteed to fit within a single 8-byte pointer.
template<typename First, typename Second>
class t_ptr {
private:
    constexpr static uint64_t TAG_MASK = 1;
    constexpr static uint64_t POINTER_MASK = ~TAG_MASK;

    intptr_t bits_;

    void* pointer() const {
        return reinterpret_cast<void*>(bits_ & POINTER_MASK);
    }

    int tag() const {
        return bits_ & TAG_MASK;
    }

    template<typename T>
    static constexpr bool validType() {
        return std::is_same_v<T, First> || std::is_same_v<T, Second>;
    }

    template<typename T>
    void set(T* pointer) {
        static_assert(validType<T>());
        bits_ = reinterpret_cast<intptr_t>(pointer);
        bits_ |= std::is_same_v<T, First>;
    }

public:
    t_ptr() {
        bits_ = 0;
    }

    template<typename T>
    t_ptr(T* pointer) {
        set(pointer);
    }

    template<typename T>
    bool holds() const {
        static_assert(validType<T>());
        return tag() == std::is_same_v<T, First>;
    }

    template<typename T>
    T* get() const {
        static_assert(validType<T>());
        return holds<T>() ? reinterpret_cast<T*>(pointer()) : nullptr;
    }

    template<typename T>
    t_ptr& operator=(T* pointer) {
        static_assert(validType<T>());
        assert(bits_ == 0);
        set(pointer);
    }
};

// enhancement: Add memoization and interning layer in the arena. As its primary use will be storing tokens/ast nodes on
// the heap, memoization will reduce memory usage in the heap by a fair amount. For example each whitespace mode will
// take up 24 bytes in the arena.
struct arena {
    constexpr static std::size_t INITIAL_SIZE = 1024 * 64;

protected:
    void* memory_;
    std::size_t size_ = 0;
    std::size_t offset_ = 0;

public:
    explicit arena(std::size_t initialSize = INITIAL_SIZE) {
        memory_ = std::malloc(initialSize);
        size_ = initialSize;
        offset_ = 0;
    }

    arena(const arena& other) = delete;
    arena operator =(const arena& other) = delete;

    arena(arena&& other) noexcept {
        memory_ = other.memory_;
        size_ = other.size_;
        offset_ = other.offset_;

        other.memory_ = nullptr;
    }

    arena& operator =(arena&& other) noexcept {
        std::swap(memory_, other.memory_);
        std::swap(size_, other.size_);
        std::swap(offset_, other.offset_);
        return *this;
    }

    ~arena() {
        if (memory_) {
            std::free(memory_);
        }
    }

    template<typename T, class... Args>
    T* alloc(Args&& ... args) noexcept {
        // Means we do not need to call destructors on these objects.
        static_assert(std::is_trivially_destructible_v<T>);

        constexpr auto alignment = alignof(T);
        constexpr auto size = sizeof(T);

        if (auto* memory = alloc(size, alignment)) {
            return new(memory) T(std::forward<Args>(args)...);
        }

        return nullptr;
    }

    void* alloc(std::size_t size, std::size_t alignment = 8) noexcept {
        auto sizeRemaining = size_ - offset_;
        void* dest = static_cast<std::byte*>(memory_) + offset_;

        void* res = std::align(alignment, size, dest, sizeRemaining);
        if (res) {
            sizeRemaining -= size;
            offset_ = size_ - sizeRemaining;
            return res;
        }

        return nullptr;
    }

    std::size_t size() const {
        return size_;
    }

    std::size_t offset() const {
        return offset_;
    }

    const void* memory() const {
        return memory_;
    }
};

}

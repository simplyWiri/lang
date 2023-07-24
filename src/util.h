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
        set(pointer);
    }
};

}
#include <doctest.h>
#include "util.h"

TEST_SUITE_BEGIN("Utility Tests");

TEST_CASE("Tagged Pointer") {
    int integerValue = 5;
    util::t_ptr<int, float> intPointer{&integerValue};

    CHECK(intPointer.holds<int>());
    CHECK_EQ(*intPointer.get<int>(), integerValue);
    CHECK_EQ(intPointer.get<float>(), nullptr);

    float floatValue = 6.0;
    util::t_ptr<int, float> floatPointer{&floatValue};

    CHECK(floatPointer.holds<float>());
    CHECK_EQ(*floatPointer.get<float>(), floatValue);
    CHECK_EQ(floatPointer.get<int>(), nullptr);

    SUBCASE("operator=") {
        util::t_ptr<int, float> a;
        a = intPointer;
        CHECK(a.holds<int>());

        util::t_ptr<int, float> b;
        b = floatPointer;
        CHECK(b.holds<float>());
    }
}

TEST_CASE("Arena Construction") {
    SUBCASE("Default Constructor") {
        util::arena allocator;
        CHECK_EQ(allocator.size(), util::arena::INITIAL_SIZE);
    }

    SUBCASE("Custom Initial Size") {
        util::arena allocator{ 512 };
        CHECK_EQ(allocator.size(), 512);
    }
}

TEST_CASE("Arena Allocation") {
    constexpr static auto SIZE = 512;
    util::arena allocator{SIZE};

    CHECK_EQ(allocator.size(), SIZE);
    CHECK_EQ(allocator.offset(), 0);

    const auto numIntegers = SIZE / sizeof(int64_t);

    SUBCASE("Strong Typed Interface") {
        for (std::size_t i = 0; i < numIntegers; i++) {
            CHECK(allocator.alloc<int64_t>(i));
            CHECK_EQ(allocator.offset(), (i + 1) * sizeof(int64_t));
        }

        // when offset == size(), there is no more memory to allocate.
        CHECK_EQ(allocator.offset(), allocator.size());
        CHECK_EQ(allocator.alloc<int>(), nullptr);
    }

    SUBCASE("Raw Interface") {
        for (std::size_t i = 0; i < numIntegers; i++) {
            CHECK(allocator.alloc(sizeof(int64_t), alignof(int64_t)));
            CHECK_EQ(allocator.offset(), (i + 1) * sizeof(int64_t));
        }

        // when offset == size(), there is no more memory to allocate.
        CHECK_EQ(allocator.offset(), allocator.size());
        CHECK_EQ(allocator.alloc<int>(), nullptr);
    }
}

TEST_CASE("Arena Move Operations") {
    constexpr static auto SIZE = 512;
    util::arena firstAlloc{SIZE};
    firstAlloc.alloc<int>();

    const auto size = firstAlloc.size();
    const auto offset = firstAlloc.offset();
    const auto* memory = firstAlloc.memory();

    SUBCASE("Operator Equals") {
        util::arena secondAlloc;
        secondAlloc = std::move(firstAlloc);

        CHECK_EQ(secondAlloc.size(), size);
        CHECK_EQ(secondAlloc.offset(), offset);
        CHECK_EQ(secondAlloc.memory(), memory);
    }

    SUBCASE("Move Constructor") {
        util::arena secondAlloc{std::move(firstAlloc)};

        CHECK_EQ(secondAlloc.size(), size);
        CHECK_EQ(secondAlloc.offset(), offset);
        CHECK_EQ(secondAlloc.memory(), memory);
    }
}

TEST_SUITE_END();
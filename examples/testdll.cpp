#include <windows.h>

extern "C" long testfunc1(long x, long y, long z)
{
	return x * y + z;
}

extern "C" long testfunc2(long x, void (*func)(long))
{
	if (x < 0 || x > 100)
		return 0;
	int i;
	for (i = 0; i < x; i++)
		func(i);
	return i + i;
}

extern "C" double testfunc3(double x, double y, double z)
{
	return x * y + z;
}

extern "C" __int64 testfunc4(__int32 hi, __int32 lo)
{
    __int64 result = (__int64)hi;

    result = result<<32;
    result += (__int64)lo;

	return result;
}

extern "C" __int64 testfunc5(__int64 x, __int64 y)
{
    __int64 result = x + y;
	return result;
}

extern "C" unsigned __int64 testfunc6(unsigned __int64 x, unsigned __int64 y)
{
    unsigned __int64 result = x + y;
	return result;
}

class testclass
{
	__declspec(dllexport) testclass(long num);
	__declspec(dllexport) int mulsub(long x, long y, long z);
private:
	long num_;
};

__declspec(dllexport) testclass::testclass(long num)
{
	num_ = num;
}

__declspec(dllexport) int testclass::mulsub(long x, long y, long z)
{
	return num_ + x * y - z;
}

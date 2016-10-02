//		File:		UserInfo.cpp
//		Contents:	User specific information class partial implementation.
//		History:	10/01/16  Artem Boldarev Created.
//					           
//

#include "UserInfo.h"

#include <windows.h>

UserInfo::UserInfo(void)
: name(NULL), version(31)
{
}

UserInfo::~UserInfo(void)
{
	if (name != NULL)
		delete[] name;
}

const char *UserInfo::GetName(void)
{
	return name == NULL ? "" : name;
}


int UserInfo::GetVersion(void)
{
	return version;
}

bool UserInfo::FillUserInfo(UserInfo &pUserInfo)
{
	// obraining user name
	DWORD user_name_size = 0;
	char *name_buf = NULL;
	char *organization_buf = NULL;

	// get buffer size
	GetUserNameA(NULL, &user_name_size);
	if (user_name_size == 0)
		return false;

	// allocate buffer for user name
	name_buf = new char[user_name_size];
	if (!GetUserNameA(name_buf, &user_name_size))
	{
		delete[] name_buf;
		name_buf = NULL;
		return false;
	}

	// fill UserInfo
	pUserInfo.name = name_buf;

	return true;
}

//		File:		UserInfo.cpp
//		Contents:	User specific information class partial implementation.
//		History:	10/01/16  Artem Boldarev Created.
//					10/04/16  Artem Boldarev
//							  User profile directory retrieval.
//					10/05/16  Artem Boldarev
//							  Added length retrieving methods.
//

#include "UserInfo.h"

#include <windows.h>
#include <userenv.h>

UserInfo::UserInfo(void)
: name(NULL), name_len(0),
profile_directory(NULL), profile_directory_len(0),
version(31)
{
}

UserInfo::~UserInfo(void)
{
	if (name != NULL)
		delete[] name;

	if (profile_directory != NULL)
		delete[] profile_directory;
}

const char *UserInfo::GetName(void) const
{
	return name == NULL ? "" : name;
}

size_t UserInfo::GetNameLength(void) const
{
	return name_len;
}


const char *UserInfo::GetProfileDirectory(void) const
{
	return profile_directory == NULL ? "" : profile_directory;
}

size_t UserInfo::GetProfileDirectoryLength(void) const
{
	return profile_directory_len;
}

int UserInfo::GetVersion(void) const
{
	return version;
}

bool UserInfo::FillUserInfo(UserInfo &ui)
{
	// obraining user name
	DWORD user_name_size = 0;
	DWORD user_profile_buffer_size = 0;
	char *name_buf = NULL;
	char *profile_buf = NULL;
	bool user_name_status = true;
	bool user_profile_status = true;

	/// !!! Get currently logged in user login name. !!!
	// get buffer size
	GetUserNameA(NULL, &user_name_size);
	if (user_name_size == 0)
	{
		user_name_status = false;
	}

	if (user_name_status)
	{
		// allocate buffer for user name
		name_buf = new char[user_name_size];
		if (!GetUserNameA(name_buf, &user_name_size))
		{
			delete[] name_buf;
			name_buf = NULL;
			user_name_status = false;
		}
	}

	/// !!! Get currently logged in user profile directory path. !!!
	{
		// get user token handle
		HANDLE user_token = NULL;
		BOOL res = OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &user_token);
		if (res == FALSE || user_token == NULL)
		{
			user_profile_status = false;
		}

		// get user profile path itself 
		if (user_profile_status)
		{
			// get buffer size
			char tmp = '\0';
			// It is strange, but at least on Windows 10 one have to specify some dummy pointer as the path parameter
			// for GetUserProfileDirectoryA. MSDN says nothing about it.
			GetUserProfileDirectoryA(user_token, &tmp, &user_profile_buffer_size);
			if (user_profile_buffer_size == 0)
			{
				user_profile_status = false;
			}

			// get path
			if (user_profile_status)
			{
				profile_buf = new char[user_profile_buffer_size + 1]; // for '\\' at the end
				res = GetUserProfileDirectoryA(user_token, profile_buf, &user_profile_buffer_size);
				if (res == FALSE || user_profile_buffer_size == 0)
				{
					delete[] profile_buf;
					user_profile_status = false;
				}
				else
				{
					if (profile_buf[user_profile_buffer_size - 1] != '\\')
					{
						strcat_s(profile_buf, user_profile_buffer_size + 1, "\\");
						user_profile_buffer_size += 1;
					}
				}
			}
		}

		if (user_token != NULL)
		{
			CloseHandle(user_token);
		}
	}

	// fill UserInfo
	if (user_name_status)
	{
		ui.name = name_buf;
		ui.name_len = user_name_size - 1;
	}

	if (user_profile_status)
	{
		ui.profile_directory = profile_buf;
		ui.profile_directory_len = user_profile_buffer_size - 1;
	}

	return user_name_status && user_profile_status;
}

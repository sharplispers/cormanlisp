//		File:		UserInfo.h
//		Contents:	User specific information class description.
//		History:	10/01/16  Artem Boldarev Created.
//					           
//

#ifndef USERINFO_H
#define USERINFO_H

#include <stddef.h>

class UserInfo
{
public:
	UserInfo(void);
	~UserInfo(void);

	static bool FillUserInfo(UserInfo &pUserInfo);

	const char *GetName(void);
	int GetVersion(void);
private:
	// non copyable
	UserInfo(const UserInfo &);
	void operator=(const UserInfo &);
private:
	char *name;
	int version;
};

#endif /* USERINFO_H */

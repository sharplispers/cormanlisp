//		File:		UserInfo.h
//		Contents:	User specific information class description.
//		History:	10/01/16  Artem Boldarev Created.
//					10/04/16  Artem Boldare
//							  User profile directory retrieval.
//					           


#ifndef USERINFO_H
#define USERINFO_H

#include <stddef.h>

class UserInfo
{
public:
	UserInfo(void);
	~UserInfo(void);

	static bool FillUserInfo(UserInfo &ui);

	const char *GetName(void) const ;
	const char *GetProfileDirectory(void) const;
	int GetVersion(void) const;
private:
	// non copyable
	UserInfo(const UserInfo &);
	void operator=(const UserInfo &);
private:
	char *name;
	char *profile_directory;
	int version;
};

#endif /* USERINFO_H */

//		File:		UserInfo.h
//		Contents:	User specific information class description.
//		History:	10/01/16  Artem Boldarev Created.
//					10/04/16  Artem Boldarev           
//							  User profile directory retrieval.
//					10/05/16  Artem Boldarev
//							  Added length retrieving methods.
//					10/06/16  Artem Boldarev
//							  Personal (e.g. Documents) directory retrieval code.
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
	size_t GetNameLength(void) const;
	const char *GetProfileDirectory(void) const;
	size_t GetProfileDirectoryLength(void) const;
	const char *GetPersonalDirectory(void) const;
	size_t GetPersonalDirectoryLength(void) const;

	int GetVersion(void) const;
private:
	// non copyable
	UserInfo(const UserInfo &);
	void operator=(const UserInfo &);
private:
	char *name;
	size_t name_len;
	char *profile_directory;
	size_t profile_directory_len;
	char *personal_directory;
	size_t personal_directory_len;
	int version;
};

#endif /* USERINFO_H */

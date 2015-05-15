#define DllExport   extern "C" __declspec( dllexport )

#include "phonenumbers/phonenumberutil.h"

DllExport int64_t _StrPhoneNumberToInt64( const char* phoneNumber, const char* countryCode ) {
	using i18n::phonenumbers::PhoneNumberUtil;
	using i18n::phonenumbers::PhoneNumber;

	std::string phoneNumberStr(phoneNumber);
	std::string countryCodeStr(countryCode);
	std::string *formatted_number = new std::string();

	const PhoneNumberUtil& phone_util(*PhoneNumberUtil::GetInstance());
	PhoneNumber *num = new PhoneNumber();
	phone_util.Parse(phoneNumberStr, countryCodeStr, num);
	phone_util.Format(*num, PhoneNumberUtil::E164, formatted_number);

	int64_t result = 0;
	if(formatted_number->length() > 0)
	{
		*formatted_number = formatted_number->substr(1, std::string::npos);
		result = _atoi64(formatted_number->c_str());
	}
	delete formatted_number;
	delete num;
	return(result);
}

DllExport void _Int64PhoneNumberToStr( int64_t phoneNumber,
	                                    char *buffer ) {
	using i18n::phonenumbers::PhoneNumberUtil;
	using i18n::phonenumbers::PhoneNumber;

	std::string phoneNumberStr = "+" + std::to_string(phoneNumber);
	std::string formatted_string(buffer);

	const PhoneNumberUtil& phone_util(*PhoneNumberUtil::GetInstance());
	PhoneNumber *num = new PhoneNumber();
	phone_util.Parse(phoneNumberStr, "ZZ", num); // ZZ means no defined country, possible to use if number started from +
	phone_util.Format(*num, PhoneNumberUtil::INTERNATIONAL, &formatted_string);
	
	delete num;
	strcpy(buffer, formatted_string.c_str());
}


DllExport long _GetPhoneNumberType( int64_t phoneNumber ) {
	using i18n::phonenumbers::PhoneNumberUtil;
	using i18n::phonenumbers::PhoneNumber;

	std::string phoneNumberStr = "+" + std::to_string(phoneNumber);
	
	const PhoneNumberUtil& phone_util(*PhoneNumberUtil::GetInstance());
	PhoneNumber *num = new PhoneNumber();
	phone_util.Parse(phoneNumberStr, "ZZ", num); // ZZ means no defined country, possible to use if number started from +
	long phone_type = phone_util.GetNumberType(*num);
	delete num;
	return(phone_type);
}
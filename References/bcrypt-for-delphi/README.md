Bcrypt for Delphi
==================

[Bcrypt](http://en.wikipedia.org/wiki/Bcrypt) is an algorithm designed for hashing passwords, and only passwords; i.e. it:

- is **not** a high-speed, generic, hashing algorithm
- is **not** a key derivation function (see [PBDKF2](http://en.wikipedia.org/wiki/PBKDF2), [scrypt](http://en.wikipedia.org/wiki/Scrypt))
- is computationally and memory expensive
- is limited to passwords of 55 bytes

It was first [described by Niels Provos and David Mazières in 1999](http://static.usenix.org/events/usenix99/provos/provos.pdf) for OpenBSD.

It uses the Blowfish encryption algorithm, but with an "expensive key setup" modification, contained in the function `EksBlowfishSetup`.

Sample Usage
----------------

- To hash a password:

        hash := TBCrypt.HashPassword('correct battery horse staple'); //using default cost factor
    
- To hash a password specifying your own cost factor:

        hash := TBCrypt.HashPassword('correct battery horse staple', 14); //specify cost factor 14
    
- To verify a password:

        passwordRehashNeeded: Boolean;
        isPasswordValid := TBCrypt.CheckPassword('correct battery horse stapler', expectedHash, {out}passwordRehashNeeded);
	
The out parameter `passwordRehashNeeded` indicates if the stored password hash needs to be upgraded. A hash would need to be upgraded if:

- the password hash took less than 250 ms to compute
- the stored hash version (e.g. `2`, `2x`, `2y`) needs to be upgraded (i.e. `2a`, soon to be `2b`)

    
By convention BCrypt outputs a hash as string such as:

    $2a$12$EA6qjRCeBi8bGgs4rhfn8udEGKmu0ayrZYCEJqf6nNIoytowKFncm

The parts of the string are:

| Value | Meaning | Notes |
|-------|---------|-------|
| 2a | Hash algorithm | "2a" = current version of BCrypt, "2" = obsolete version of BCrypt, "1" = MD5 |
| 12 | cost factor | Will iterate for 2<sup>12</sup>=4,096 rounds. (Default is 11) |
| EA6qjRCeBi8bGgs4rhfn8u | Salt | 22 base64 encoded characters |
| dEGKmu0ayrZYCEJqf6nNIoytowKFncm | Hashed password | 31 base64 encoded characters |

Because the **cost factor** is stored with the hash output, bcrypt hashes are backwards and forwards compatible with
	changing the number of rounds. It also makes BCrypt extraordinarily convenient in that a random salt is automatically generated and stored for you (you don't have to worry about storing or retrieving it).

Speed Tests
--------------

The current (3/21/2015) hard-coded default for cost is **11**. This results in 2<sup>11</sup> = 2,048 rounds during the key setup.

3/14/2015  Intel Core i5-2500 CPU @ 3.50 GHz Delphi XE6 (32-bit, Release)

| Cost | Iterations        |  Duration  |
|------|-------------------|------------|
|  8   |    256 iterations |    22.0 ms | <-- minimum allowed by BCrypt
|  9   |    512 iterations |    43.3 ms |
| 10   |  1,024 iterations |    85.5 ms |
| 11   |  2,048 iterations |   173.3 ms | <-- current default (BCRYPT_COST=11)
| 12   |  4,096 iterations |   345.6 ms |
| 13   |  8,192 iterations |   694.3 ms |
| 14   | 16,384 iterations | 1,390.5 ms |
| 15   | 32,768 iterations | 2,781.4 ms |
| 16   | 65,536 iterations | 5,564.9 ms |


At the time of publication of BCrypt (1999) the default costs were:

- Normal User: 6
- the Superuser: 8

> *"Of course, whatever cost people choose should be reevaluated from time to time."*

- At the time of deployment in 1976, **crypt** could hash fewer than 4 passwords per second. (250 ms per password)  
- In 1977, on a VAX-11/780, crypt (MD5) could be evaluated about 3.6 times per second.   (277 ms per password)

We want to target between 250-500 ms per hash. To that end, when calling `HashPassword` the system will automatically determine a cost factor that results in a hash that takes 250-500 ms to compute. It does this by profiling the computer performance. Regardless of the results of the profiling, it will never use a cost lower than the `BCRYPT_COST` constant.

Bcrypt variants
-------------

- **$2$** *(June 1999)*

    The original specification used the prefix `$2$`.

    This was in contrast to the other algorithm prefixes in the OpenBSD password file, e.g.:

    - **`$1$`**: [MD5](https://en.wikipedia.org/wiki/MD5)
    - **`$5$`**: [SHA2](https://en.wikipedia.org/wiki/SHA-2)-256
    - **`$6$`**: [SHA2](https://en.wikipedia.org/wiki/SHA-2)-512
    

- **$2a$**

    The original specification did not define how to handle non-ASCII character, or how to handle a null terminator.   
    The specification was revised to specify that when hashing strings:
    
    - the string must be UTF-8 encoded
    - the null terminator must be included
    
- **$2x$**, **$2y$** *(June 2011)*

    A [bug was discovered](http://seclists.org/oss-sec/2011/q2/632) in [crypt_blowfish](https://pear.php.net/package/Crypt_Blowfish), a PHP implementation of BCrypt. It was mis-handling characters with the 8th bit set.
    
    They suggested that system administrators update their existing password database: replacing `$2a$` with `$2x$`, to indicate that those hashes are bad (and need to use the old broken algorithm).  
    They also suggested the idea of having crypt_blowfish emit `$2y$` for hashes generated by the fixed algorithm. Nobody else, including canonical OpenBSD, adopted the idea of 2x/2y. This version marker was was limited to crypt_blowfish.

- **$2b$** *(February 2014)*
 
     A [bug was discovered](http://undeadly.org/cgi?action=article&sid=20140224132743) in the canonical OpenBSD implemenation of bcrypt.
     
     They were storing the length of their strings in an unsigned char.
     If a password was longer than 255 characters, it would overflow and wrap at 255.
     BCrypt was created for OpenBSD. When they have a bug in *their* library, they decided its ok to bump the version.
     This means that everyone else needs to follow suit if you want to remain current to "their" specification.
     
     **Bonus Reading**: OpenBSD mailing list item [*"bcrypt version changes"*](http://marc.info/?l=openbsd-misc&m=139320023202696) (2/24/2014)


Created by [Ian Boyd 5/3/2012](http://stackoverflow.com/a/10441765/9990)

Public Domain  
For more information, please refer to <http://unlicense.org/>

**Note**: There is also [Scrypt for Delphi](https://github.com/JoseJimeniz/scrypt-for-delphi).



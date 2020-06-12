makecert -sv SignRoot.pvk -cy authority -r signroot.cer -a sha1 -n "CN=Dev Certification Authority" -ss my -sr localmachine
makecert -iv SignRoot.pvk -ic signroot.cer -cy end -pe -n CN="rest.server.com" -eku 1.3.6.1.5.5.7.3.1 -ss my -sr localmachine -sky exchange -sp "Microsoft RSA SChannel Cryptographic Provider" -sy 12

rem netsh http delete sslcert ipport=0.0.0.0:8443

netsh http add sslcert ipport=0.0.0.0:8443 1600ccafee284bdeec7622ad5534881310b2e0f0 appid={00112233-4455-6677-8899-AABBCCDDEEFF}

#!/bin/bash
APPDIR="app/rpg_battlemap"

function pre_clean {
	rm -f $APPDIR/include/commit_ver.hrl
}

function pre_compile {
	if [ ! -d $APPDIR/ebin ]; then
		mkdir $APPDIR/ebin
	fi

	if [ ! -d $APPDIR/priv ]; then
		mkdir $APPDIR/priv
	fi

	if [ ! -d $APPDIR/include ]; then
		mkdir $APPDIR/include
	fi

	# build certificates if needed
	if [ ! -f $APPDIR/priv/key ]; then
		echo "RSA key does not exist, generating..."
		ssh-keygen -t rsa -f $APPDIR/priv/key -N ""
		RES=$?
		if [ $RES != 0 ]; then
			echo "Key generation failed with error $RES!"
			exit $RES
		fi
	fi

	if [ ! -f $APPDIR/priv/rpgb.csr ]; then
		echo "Certificate Signing Request does not exist, generating..."
		openssl req -new -key $APPDIR/priv/key -out $APPDIR/priv/rpgb.csr
		RES=$?
		if [ $RES != 0 ]; then
			echo "CSR generation failed with error $RES"
			exit $RES
		fi
	fi

	if [ ! -f $APPDIR/priv/rpgb.crt ]; then
		echo "Certificate does not exists, generating self-signed for a year..."
		openssl x509 -req -days 365 -in $APPDIR/priv/rpgb.csr -signkey $APPDIR/priv/key -out $APPDIR/priv/rpgb.crt
		RES=$?
		if [ $RES != 0 ]; then
			echo "Certificate generation failed with error $RES"
			exit $RES
		fi
	fi

	# record what commit/version the rep is at
	COMMIT=""
	if [ -d ".git" ]
	then
		COMMIT=`git log -1 --pretty=format:%H`
	fi
	if [ -e "$APPDIR/include/commit_ver.hrl" ] && [ ! $COMMIT ]
	then
		exit 0
	else
		if [ ! COMMIT ]
		then
			COMMIT="undefined"
		else
			COMMIT="\"$COMMIT\""
		fi
	fi
	echo "%% automatically generated by precompile script.  Editing means this
%% will just be overwritten.

-define(COMMIT, $COMMIT)." > $APPDIR/include/commit_ver.hrl

}

function post_compile {
	cat success_message
}

case $1 in
	"pre_compile")
		pre_compile;;
	"post_compile")
		post_compile;;
	"pre_clean")
		pre_clean;;
	*)
		RED="\033[1;31m"
		RESET="\033[m"
		echo -e $RED"hooks.sh: Unknown hook '$1'!"$RESET
esac

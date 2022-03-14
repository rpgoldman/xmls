system		:= xmls
webhome_dir	:= /project/${system}/public_html/
webhome_private := common-lisp.net:${webhome_dir}
webhome_public	:= "http://common-lisp.net/project/${system}/"
sourceDirectory := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
webfiles	:= web-page/clnet-page.shtml web-page/styles.css README.html
ifeq (${user},)
userat :=
else
userat := ${user}@
endif
website:=${userat}common-lisp.net:/project/${system}/public_html/
version := $(shell cat "version.lisp-expr")
XMLSDIR := "${system}-$(version)"
TARBALL := "build/${XMLSDIR}.tar.gz"



.PHONY: archive publish-archive website publish-latest

archive: ;
	mkdir -p build
	git archive --output ${TARBALL} --prefix 'xmls/' HEAD
	gpg -o ${TARBALL}.asc --sign ${TARBALL}
	md5sum --binary ${TARBALL} > ${TARBALL}.md5

# must be done after archive
publish-archive:
	$(eval GPGSIG := ${TARBALL}.asc)
	$(eval MD5SUM := ${TARBALL}.md5)
	rsync --times --chmod=a+rX,ug+w ${TARBALL} ${GPGSIG} ${MD5SUM}  ${website}
	ssh common-lisp.net "cd ${webhome_dir}; ln -sf ${TARBALL} latest.tar.gz; ln -sf ${GPGSIG} latest.tar.gz.asc; ln -sf ${MD5SUM} latest.tar.gz.md5;"

# must be done after archive
publish-latest:
	rsync --times --recursive --chmod=a+rX,ug+w build/${XMLSDIR} ${website}
	ssh common-lisp.net "cd ${webhome_dir}; ln -sf ${XMLSDIR} latest;"

website: ;
	rsync -lt --no-g ${webfiles} ${website}
	ssh common-lisp.net "cd ${webhome_dir}; cp clnet-page.shtml index.shtml;"

publish: archive publish-archive publish-latest website

clean:
	rm -r build


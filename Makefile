all: .sftp_mitm_touched .sftp_mem_touched

dist/build/sftp-mitm/sftp-mitm dist/build/sftp-mem/sftp-mem: \
  bin/sftp-mitm.hs Network/SFTP/Messages.hs \
  bin/sftp-mem.hs Network/SFTP/Messages.hs
	./build.sh

images/sftp-mitm/sftp-mitm: dist/build/sftp-mitm/sftp-mitm
	cp dist/build/sftp-mitm/sftp-mitm images/sftp-mitm/

images/sftp-mem/sftp-mitm: dist/build/sftp-mitm/sftp-mitm
	cp dist/build/sftp-mitm/sftp-mitm images/sftp-mem/

images/sftp-mem/sftp-mem: dist/build/sftp-mem/sftp-mem
	cp dist/build/sftp-mem/sftp-mem images/sftp-mem/

.sftp_mitm_touched: images/sftp-mitm/Dockerfile images/sftp-mitm/sftp-mitm
	docker build -t sftp-mitm images/sftp-mitm/
	touch .sftp_mitm_touched

.sftp_mem_touched: images/sftp-mem/Dockerfile images/sftp-mem/sftp-mitm images/sftp-mem/sftp-mem
	docker build -t sftp-mem images/sftp-mem/
	touch .sftp_mem_touched

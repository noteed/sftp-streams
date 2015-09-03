all: .sftp_mitm_touched .sftp_read_touched

dist/build/sftp-mitm/sftp-mitm dist/build/sftp-read/sftp-read: \
  bin/sftp-mitm.hs Network/SFTP/Messages.hs \
  bin/sftp-read.hs Network/SFTP/Messages.hs
	./build.sh

images/sftp-mitm/sftp-mitm: dist/build/sftp-mitm/sftp-mitm
	cp dist/build/sftp-mitm/sftp-mitm images/sftp-mitm/

images/sftp-read/sftp-mitm: dist/build/sftp-mitm/sftp-mitm
	cp dist/build/sftp-mitm/sftp-mitm images/sftp-read/

images/sftp-read/sftp-read: dist/build/sftp-read/sftp-read
	cp dist/build/sftp-read/sftp-read images/sftp-read/

.sftp_mitm_touched: images/sftp-mitm/Dockerfile images/sftp-mitm/sftp-mitm
	docker build -t sftp-mitm images/sftp-mitm/
	touch .sftp_mitm_touched

.sftp_read_touched: images/sftp-read/Dockerfile images/sftp-read/sftp-mitm images/sftp-read/sftp-read
	docker build -t sftp-read images/sftp-read/
	touch .sftp_read_touched

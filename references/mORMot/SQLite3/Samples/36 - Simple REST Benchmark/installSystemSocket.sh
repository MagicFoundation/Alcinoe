ln -s "`pwd`/rest_benchmark.socket" /etc/systemd/system
ln -s "`pwd`/rest_benchmark.service" /etc/systemd/system
systemctl enable rest_benchmark.socket
systemctl start rest_benchmark.socket
systemctl status rest_benchmark.socket #output status to console
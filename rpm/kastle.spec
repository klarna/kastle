%define debug_package %{nil}
%define _service      %{_name}
%define _user         %{_name}
%define _group        %{_name}
%define _prefix      /opt
%define _conf_dir    %{_sysconfdir}/%{_service}
%define _log_dir     /var/log/%{_service}

Summary: %{_description}
Name: %{_name}
Version: %{_version}
Release: 1%{?dist}
License: Apache License, Version 2.0
URL: https://github.com/klarna/kastle
BuildRoot: %{_tmppath}/%{_name}-%{_version}-root
Prefix: %{_prefix}
Prefix: %{_conf_dir}
Prefix: %{_log_dir}
Vendor: Klarna AB
Packager: Ivan Dyachkov <ivan.dyachkov@klarna.com>
Provides: %{_service}
BuildRequires: systemd
%systemd_requires

%description
%{_description}

%prep

%build

%install
mkdir -p %{buildroot}%{_prefix}
mkdir -p %{buildroot}%{_log_dir}
mkdir -p %{buildroot}%{_unitdir}
mkdir -p %{buildroot}%{_conf_dir}
mkdir -p %{buildroot}%{_sysconfdir}/sysconfig
mkdir -p %{buildroot}/usr/local/bin
cp -r _rel/%{_name}  %{buildroot}%{_prefix}/
%{__install} -p -D -m 0644 rpm/kastle.service %{buildroot}%{_unitdir}/%{_service}.service
erl -noshell -eval '{ok, [Config0]} = file:consult("rel/sys.config"), LagerConfig0 = proplists:get_value(lager, Config0, []), LagerConfig1 = lists:keystore(log_root, 1, LagerConfig0, {log_root, "%{_log_dir}"}), LagerConfig = lists:keystore(handlers, 1, LagerConfig1, {handlers, [{lager_console_backend,error},{lager_file_backend,[{file,"error.log"},{level,error},{size, 10485760}, {date, "$W0D23"}, {count, 5}]}]}), Config1 = lists:keystore(lager, 1, Config0, {lager, LagerConfig}), KastleConfig0 = proplists:get_value(kastle, Config1, []), KastleConfig = lists:keystore(listeners, 1, KastleConfig0, {listeners, 64}), Config = lists:keystore(kastle, 1, Config1, {kastle, KastleConfig}), file:write_file("rel/sys.config", io_lib:format("~p.~n", [Config])), {ok, _} = file:consult("rel/sys.config"), halt(0).'
%{__install} -p -D -m 0644 rel/sys.config %{buildroot}%{_conf_dir}/sys.config
%{__install} -p -D -m 0644 rel/vm.args %{buildroot}%{_conf_dir}/vm.args

cat > %{buildroot}%{_sysconfdir}/sysconfig/%{_service} <<EOF
RUNNER_LOG_DIR=%{_log_dir}
RELX_CONFIG_PATH=%{_sysconfdir}/%{_service}/sys.config
VMARGS_PATH=%{_sysconfdir}/%{_service}/vm.args
EOF

cat > %{buildroot}/usr/local/bin/%{_service} <<EOF
#!/bin/sh
source %{_sysconfdir}/sysconfig/%{_service}
%{_prefix}/%{_service}/bin/%{_service} \$@
EOF

%clean
rm -rf $RPM_BUILD_ROOT

%pre
if [ $1 = 1 ]; then
  # Initial installation
  /usr/bin/getent group %{_group} >/dev/null || /usr/sbin/groupadd -r %{_group}
  if ! /usr/bin/getent passwd %{_user} >/dev/null ; then
      /usr/sbin/useradd -r -g %{_group} -c "%{_name}" %{_user}
  fi
fi

%post
%systemd_post %{_service}.service

%preun
%systemd_preun %{_service}.service

%postun
%systemd_postun

%files
%defattr(-,root,root)
%{_prefix}/%{_service}
%attr(0755,root,root) /usr/local/bin/%{_service}
%{_unitdir}/%{_service}.service
%config(noreplace) %{_conf_dir}/*
%config(noreplace) %{_sysconfdir}/sysconfig/%{_service}
%attr(0755,%{_user},%{_group}) %dir %{_log_dir}

%define debug_package %{nil}
%define _service      %{_name}
%define _user         %{_name}
%define _group        %{_name}
%define _prefix      /opt
%define _conf_dir    %{_sysconfdir}/%{_name}
%define _log_dir     /var/log/%{_name}

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
Provides: %{_name}
BuildRequires: systemd
%systemd_requires

%description
%{_description}

%prep

%build

%install
mkdir -p $RPM_BUILD_ROOT%{_prefix}
mkdir -p $RPM_BUILD_ROOT%{_log_dir}
mkdir -p $RPM_BUILD_ROOT%{_unitdir}
mkdir -p $RPM_BUILD_ROOT%{_conf_dir}
cp -r _rel/kastle %{buildroot}%{_prefix}/
install -p -D -m 0644 rpm/%{_service}.service %{buildroot}%{_unitdir}/%{_service}.service
erl -noshell -eval '{ok, [Config0]} = file:consult("rel/sys.config"), LagerConfig0 = proplists:get_value(lager, Config0, []), LagerConfig = lists:keystore(log_root, 1, LagerConfig0, {log_root, "%{_log_dir}"}), Config = lists:keystore(lager, 1, Config0, {lager, LagerConfig}), file:write_file("rel/sys.config", io_lib:format("~p.~n", [Config])), halt(0).'
install -p -D -m 0644 rel/sys.config %{buildroot}%{_conf_dir}/sys.config
install -p -D -m 0644 rel/vm.args %{buildroot}%{_conf_dir}/vm.args

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
%{_prefix}/%{_name}
%{_unitdir}/%{_service}.service
%config(noreplace) %{_conf_dir}/*
%attr(0755,%{_user},%{_group}) %dir %{_log_dir}

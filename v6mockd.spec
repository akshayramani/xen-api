# -*- rpm-spec -*-

%define XEN_RELEASE %(test -z "${XEN_RELEASE}" && echo unknown || echo $XEN_RELEASE)

Summary: v6mockd - XenServer test licensing daemon
Name:    v6mockd
Version: 0
Release: 0
Group:   System/Hypervisor
License: Proprietary
URL:  http://www.xen.org
Source0: v6mockd-%{version}.tar.bz2
BuildRoot: %{_tmppath}/%{name}-%{version}-root
BuildRequires: ocaml, ocaml-findlib, ocaml-camlp4, ocaml-type-conv, ocaml-getopt, omake, xapi-client-devel, ocaml-stdext-devel

%description
This package contains the mock licensing daemon, which reads license data from a file.

%prep 
%setup -q

%build
omake

%install
rm -rf %{buildroot}
DESTDIR=$RPM_BUILD_ROOT omake install-mock

%clean
rm -rf $RPM_BUILD_ROOT

%post
[ ! -x /sbin/chkconfig ] || chkconfig --add v6d

%files
%defattr(-,root,root,-)
/opt/xensource/libexec/v6d
/etc/rc.d/init.d/v6d
/tmp/mock.lic

%changelog

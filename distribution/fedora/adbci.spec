Name:	    adbci	
Version:	0.2
Release:	1%{?dist}
Summary:	Ada SQL library with simple active records impl

Group:		System Environment/Libraries
License:	GPLv3
URL:		https://github.com/john-vinters/adbci
Source0:	%{name}-%{version}.tar.gz
Patch0:     adbci-destdir.patch
Patch1:     %{name}-optflags.patch

BuildRequires:	fedora-gnat-project-common
BuildRequires: gcc-gnat
BuildRequires: postgresql-devel
#Requires:	

%description
ADBCI is a simple library for accessing SQL 
databases using Ada 2005.  It also contains 
an experimental active record implementation.

%prep
%setup -q
%patch0 -p1
%patch1 -p1


%build
export GNATOPTFLAGS="%Gnatmake_optflags"
make %{?_smp_mflags}


%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot} LIBDIR=%{buildroot}/%{_libdir}


%files
%doc



%changelog


#!/bin/bash
bin/install_perl_modules && bin/update-schema --commit && bin/make_css && commonlib/bin/gettext-makemo && bin/update-all-reports

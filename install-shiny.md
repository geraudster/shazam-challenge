# Install Shiny server

    cat > cran.asc <<EOF
    -----BEGIN PGP PUBLIC KEY BLOCK-----
    Version: SKS 1.1.5
    Comment: Hostname: keyserver.ubuntu.com
    
    mQGiBEXUFiURBADkTqPqcRYdLIguhC6fnwTvIxdkoN1UEBuPR6NYW4iJzvRSas/g5bPo5ZxE
    2i5BXiuVfYrSk/YiU+/lc0K6VYNDygbOfpBgGGhtfzYfFRTYNq8QsdD8L8BMYtOu5rYo5BYt
    a0vuantIS9mn9QnH7885uy5tX/TXO7ICYVHxnFNr2wCguNtMdz9+DRQ38n4iiHzTtj/7UHsD
    /0+0TLHHvY1FfakVinamR9oCm9uH9PmkGTy6jRnrvg5Z+TTgygiDdTBKPc1TqpFgoFtqh8G5
    DpDPbyh5GzBj8Ky1mBJb3bMwy2RUth1cztHEWI36xuCl+KrLtA4OYuCwJJZhOWDIO9aO2LW5
    kJhIwIuvSrEtOgTxpzy82g7eEzvLBADUrQ01fj+9VDrO2Vept8jtaGK+4kW3cBAG/UbOrTjt
    63VurXwyvNb6q7hKFUaVH42Fc0e64F217mutCyftPWYJwY4SR8hUmjEM/SYcezUDWWvVxmkF
    8M4rMhHa0j+q+et3mTKwgxehQO9hLUqRebnmJuwNqNJKb9izsPqmh83Zo7Q7Sm9oYW5uZXMg
    UmFua2UgKENSQU4gRGViaWFuIGFyY2hpdmUpIDxqcmFua2VAdW5pLWJyZW1lbi5kZT6IRgQQ
    EQIABgUCRdQ+nQAKCRAvD04U9kmvkJ3+AJ4xLMELB/fT1AwtR1azcH0lKg/TegCdEvtp3SUf
    aHP3Jvg2CkzTZOatfFuIRgQQEQIABgUCS4VoCgAKCRDvfNpxC67+5ZFyAKCAzgPTqM6sSMhB
    iaZbNCpiVtwDrQCgjMy+iqPm7SVOCq0XJsCCbxymfB+IXgQTEQIAHgUCRdQWJQIbAwYLCQgH
    AwIDFQIDAxYCAQIeAQIXgAAKCRAG+Q3lOBukgM09AKCuapN6slttAFRjs2/mgtaMMwO9sgCf
    ZD2au39Oo8QLXZhZTipN8c7j9mM=
    =BRgm
    -----END PGP PUBLIC KEY BLOCK-----
    EOF
    
    apt-key add cran.asc
    echo 'deb http://ftp.igh.cnrs.fr/pub/CRAN/bin/linux/debian jessie-cran3/' > /etc/apt/sources.list.d/cran.list
    aptitude update
    aptitude -y install r-base gdebi-core libssl-dev libcurl4-openssl-dev libxml2-dev
    R -e "install.packages('shiny', repos='https://cran.rstudio.com/')"
    wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.2.786-amd64.deb
    gdebi shiny-server-1.4.2.786-amd64.deb
    R -e "install.packages('rmarkdown', repos='https://cran.rstudio.com/')"
    R -e 'if (!require("devtools")) install.packages("devtools", repos="https://cran.rstudio.com/"); devtools::install_github("rstudio/packrat")'
    

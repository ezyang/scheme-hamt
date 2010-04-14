#!/bin/sh
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=all.pdf insert.pdf lookup-hit.pdf lookup-miss.pdf

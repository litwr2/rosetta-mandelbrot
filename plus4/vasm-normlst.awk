{
    if (match($0, "S[0-9][0-9]:")) {
       print substr(b, 1, length(b) - 1) $0
       b = ""
       next
    }
    b = b $0 "\n"
}
END {
    print b
}

def gcd(a,b)
    if b == 0
        return a
    else
        return gcd(b, (a % b))
    end
end


def cons(x,y)
    lambda do |f|
        f.call(x,y)
    end
end

def car(z)
    f = lambda{|x,y| x}
    z.call(f)

end

def cdr(z)
    f = lambda{|x,y| y}
    z.call(f)
end

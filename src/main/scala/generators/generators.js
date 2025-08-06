function* gen(n) {
    while (n > 0) {
       const input = yield n
       console.log("input: " + input)
        n = n - 1    
    }
    yield
}

const g = gen(4)
console.log("1: " + JSON.stringify(g.next()))
console.log("2: " + JSON.stringify(g.next("a")))
console.log("3: " + JSON.stringify(g.next("b")))
console.log("4: " + JSON.stringify(g.next("c")))
console.log("5: " + JSON.stringify(g.next("d")))
console.log("6: " + JSON.stringify(g.next("e")))
console.log("7: " + JSON.stringify(g.next("f")))
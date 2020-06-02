local function test(file)
    print("begin test => " .. file)
    require(file)
    print("end test <= " .. file)
end

test 'json_test'
test 'msgpack_test'

-- tests from lua.js
test 'bisect'
test 'cf'
test 'factorial'
test 'fib'
test 'hello'
test 'life'
test 'optest'
test 'select'
test 'sort'
test 'table'
test 'test_math'
# Kastle - Kafka REST proxy
Kastle is a REST interface to Kafka cluster for producers, powered by [Brod](https://github.com/klarna/brod), [Cowboy](https://github.com/ninenines/cowboy) and [Erlang VM](http://www.erlang.org/).

Kastle can handle both HTTP and HTTPS requests and supports mutual TLS authentication.

It can run behind proxy (e.g. nginx) but it's not a must, erlang vm handles concurrent connections and slow clients rather well.

## Quick demo

* Build Kastle (require Erlang/OTP 18.0, or later)

    make

* Start Kastle (require Docker-compose)

    make test-env

* Start Kastle (connect to your running kafka cluster)

    Edit `kafka_endpoints` etc. in `rel/sys.config` to point your kafka cluster, and run Kastle by `make run`

* cURL examples (by default kastle listens on port 8092 for http and 8093 for https requests)

```bash
curl -v -X POST -H "Content-type: application/binary" \
                -H "Kafka-Key:the-key" \
                --data "the-value" \
                http://localhost:8092/rest/kafka/v0/test-topic/0

curl -v -X POST -H "Content-type: application/binary" \
                -H "Kafka-Key:the-key" \
                --data "the-value" \
                --key priv/ssl/client.key \
                --cert priv/ssl/client.crt \
                --cacert priv/ssl/ca.crt \
                https://localhost:8093/rest/kafka/v0/test-topic/0
```

## Http request specs

Produce a message to a random partition of a specified topic:

    POST <endpoint>/rest/kafka/v0/<topic>

Produces a message to a specified partition of a specified topic.

    POST <endpoint>/rest/kafka/v0/<topic>/<partition>

### HTTP headers/body

Request body structure depends on headers.

#### HTTP headers option #1

    Content-Type: application/json

Request body should be a JSON object with 2 mandatory fields:

    key:   User defined key for the producing message
    value: Message value which will be produced to the topic of user choice yes

#### HTTP headers option #2

    Content-Type: application/binary
    Kafka-Key: <kafka-key-value>

Kafka-Key is an optional header, but if present its value is used as a key for kafka message as is
The whole POST request body will be taken by kastle as a binary blob and produce as kafka-value to kafka.

## Response status codes

On success, the HTTP status code in the response header is 204 No Content.

On client error, we return a JSON error object, if applicable, and the HTTP status as one of the following:

* 400 Bad Request if invalid JSON
* 400 Bad Content-Type header value, if non compliant Content-Type header
* 404 Not Found if unidentified topic or partition
* 415 Unsupported Media Type, if unsupported or no Content-Type header
* 500 If internal server fault took place
* 503 If Kafka cluster is unavailable

## Response headers

    Content-Type: application/json
    Charset: utf-8

## JSON error object

    {"error" : "error description"}

Example:

    {"error" : "partition not found"}

## Health check

    GET <endpoint>/ping

## Kastle is Ops friendly

* RPM spec is included
* Dockerfile is included
* All (well, most of them) config variables can be overriden via env variables

Build Kastle rpm package:

    make rpm

RPM includes Erlang runtime, so nothing special is required on the target system.

After installing kastle rpm an operator can use /usr/bin/kastle to talk to the node.

To give an example, we use the script below in launch configuration to initialize an AWS instance.

Just replace company.com and s3 bucket/folder with your names.

    #!/bin/bash -ex
    NAME=kastle.company.com
    CACERTFILE=/etc/ssl/certs/kastle-ca.crt
    CERTFILE=/etc/ssl/certs/kastle.crt
    KEYFILE=/etc/pki/tls/private/kastle.key
    aws s3 cp s3://s3_bucket/s3_folder/certs/${NAME}/kastle-ca.crt ${CACERTFILE}
    aws s3 cp s3://s3_bucket/s3_folder/certs/${NAME}/kastle.crt ${CERTFILE}
    aws s3 cp s3://s3_bucket/s3_folder/certs/${NAME}/kastle.key ${KEYFILE}

    sed -i "/^KASTLE_KAFKA_ENDPOINTS=/c\KASTLE_KAFKA_ENDPOINTS=kafka.company.com" /etc/sysconfig/kastle
    sed -i "/^KASTLE_ENABLE_SSL=/c\KASTLE_ENABLE_SSL=true" /etc/sysconfig/kastle
    sed -i "/^KASTLE_SSL_CACERTFILE=/c\KASTLE_SSL_CACERTFILE=${CACERTFILE}" /etc/sysconfig/kastle
    sed -i "/^KASTLE_SSL_CERTFILE=/c\KASTLE_SSL_CERTFILE=${CERTFILE}" /etc/sysconfig/kastle
    sed -i "/^KASTLE_SSL_KEYFILE=/c\KASTLE_SSL_KEYFILE=${KEYFILE}" /etc/sysconfig/kastle
    sudo systemctl start kastle

One can also set KASTLE_SSL_CIPHERS variable to comma-separated list of preferred cipher suites.

## Example of httpc to access kastle https endpoint

```erlang
1> SSLOpts =[{verify,verify_peer},{certfile,"priv/ssl/client.crt"},{keyfile, "priv/ssl/client.key"},{cacertfile,"priv/ssl/ca.crt"}].
[{verify,verify_peer},
 {certfile,"priv/ssl/client.crt"},
 {keyfile,"priv/ssl/client.key"},
 {cacertfile,"priv/ssl/ca.crt"}]
2> URL="https://localhost:8093/rest/kafka/v0/test-topic/0".
"https://localhost:8093/rest/kafka/v0/test-topic/0"
3> ContentType = "application/binary".
"application/binary"
4> Headers = [{"Kafka-Key", "the-key"}].
[{"Kafka-Key","the-key"}]
5> application:ensure_all_started(ssl).
{ok,[crypto,asn1,public_key,ssl]}
6> application:start(inets).
ok
7> httpc:request(post, {URL, Headers, ContentType, <<"the value">>}, [{ssl, SSLOpts}], []).
{ok,{{"HTTP/1.1",204,"No Content"},
     [{"date","Fri, 03 Jun 2016 08:40:45 GMT"},
      {"server","Cowboy"},
      {"content-length","0"},
      {"content-type","application/json; charset=utf-8"}],
     []}}
```

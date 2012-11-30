# hiker

A riak client for Haskell.

## Current Status

Feature matrices are based upon Basho's [Client Library Page](http://docs.basho.com/riak/1.2.0/references/Client-Libraries/)

### HTTP

| Feature               | Supported |
| --------------------- | --------- |
| List buckets          | ✗         |
| List keys             | ✗         |
| Get bucket properties | ✗         |
| Set bucket properties | ✗         |
| Fetch object          | ✗         |
| Fetch w/quorums       | ✗         |
| Store object          | ✗         |
| Store w/quorums       | ✗         |
| Delete object         | ✗         |
| Link walking          | ✗         |
| MapReduce             | ✗         |
| Secondary Indexes     | ✗         |
| Search                | ✗         |
| Ping                  | ✗         |
| Status                | ✗         |
| List Resources        | ✗         |

### Protocol Buffers

| Feature                              | Supported |
| ------------------------------------ | --------- |
| List buckets                         | ✓         |
| List keys                            | ✗         |
| Get bucket properties                | ✓         |
| Set bucket properties                | ✓         |
| Fetch object                         | ✓         |
| Fetch w/quorums                      | ✓         |
| Store object                         | ✓         |
| Store w/quorums                      | ✓         |
| Delete object                        | ✓         |
| MapReduce                            | ✗         |
| Secondary Indexes (emulated, native) | ✓ ✓       |
| Search (emulated, native)            | ✓ ✓       |
| Ping                                 | ✓         |
| Server info                          | ✓         |
| Get Client Id                        | ✓         |
| Set Client Id                        | ✓         |

### Additional features

| Feature                                | Supported         |
| -------------------------------------- | ----------------- |
| Cluster connections/pools              | single node pools |
| Retry failures (on other nodes)        | ✗                 |
| Failure-sensitive node selection       | ✗                 |
| Automatic protocol selection           | ✗                 |
| Use arbitrary media types              | ✓                 |
| JSON (de-)serialization                | ✓                 |
| Other included (de-)serializers        | ✗                 |
| Custom (de-)serializers                | ✗                 |
| Exposes siblings                       | ✗                 |
| Sibling resolution policies/strategies | ✗                 |
| Mutators (encapsulating change ops)    | ✗                 |
| Abstract domain types w/reification    | ✗                 |
| Embedded/nested domain types           | ✗                 |
| Domain-level sibling resolution        | ✗                 |
| Secondary index integration            | ✗                 |
| Search integration                     | ✗                 | 
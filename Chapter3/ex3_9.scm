
        +------------------+                            +------------------+
global  |                  |                    global  |                  |
env +-> | factorial        |                    env +-> | factorial        |
        |                  |                            |                  |
        +---------+--------+                            +---------+--------+
                  ^                                               |
                  |                                               |
                  |                                               |
             +----+----+                                   +------+-------+
       E1    |n: 6     |                               E1  |  product:1   |
         +-> |         |                                 +-+  counter:1   |
             +----+----+                                   |  max-count:6 |
                  ^                                        +------+-------+
                  |                                               |
             +----+----+                                   +------+-------+
       E2    |n: 5     |                               E2  |  product:1   |
         +-> |         |                                 +-+  counter:2   |
             +----+----+                                   |  max-count:6 |
                  ^                                        +------+-------+
                  |                                               |
             +----+----+                                   +------+-------+
       E3    |n: 5     |                               E3  |  product:2   |
         +-> |         |                                 +-+  counter:3   |
             +----+----+                                   |  max-count:6 |
                  ^                                        +------+-------+
                  |                                               |
             +----+----+                                   +------+-------+
       E4    |n: 5     |                               E4  |  product:6   |
         +-> |         |                                 +-+  counter:4   |
             +----+----+                                   |  max-count:6 |
                  ^                                        +------+-------+
                  |                                               |
             +----+----+                                   +------+-------+
       E5    |n: 5     |                               E5  |  product:24  |
         +-> |         |                                 +-+  counter:5   |
             +----+----+                                   |  max-count:6 |
                  ^                                        +------+-------+
                  |                                               |
             +----+----+                                   +------+-------+
       E6    |n: 5     |                               E6  |  product:120 |
         +-> |         |                                 +-+  counter:6   |
             +---------+                                   |  max-count:6 |
                                                           +------^-------+
                                                                  |
                                                                  |
                                                           +------+-------+
                                                       E7  |  product:720 |
                                                         +-+  counter:7   |
                                                           |  max-count:6 |
                                                           +--------------+
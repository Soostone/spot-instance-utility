# spot-instance-utility

`spot-instance-utility` is a tool over the aws sdk's
`ec2-describe-spot-price-history` tool which analyzes a window of spot
prices for the given instance types and availability zones and
produces CSV output of their stability and pricing.


## Requirements

You have to have the aws tools installed.

## Usage

```
spot-instance-utility --help                     
spot-instance-utility - Tool for selecting EC2 spot instances based on historic
prices

Usage: spot-instance-utility [-d|--duration DURATION] [INSTANCE_PAIR]
                             [-a|--availability-zone AVAILABILITY_ZONE]
                             [-p|--product-description PRODUCT_DESCRIPTION]
                             [-s|--sigmas SIGMAS]
  Analyze spot instance history for the given INSTANCE_PAIRs

Available options:
  -h,--help                Show this help text
  -d,--duration DURATION   Duration of days or weeks, such as 3d or 2w
  INSTANCE_PAIR            Pair of instance type and optional count, defaulting
                           to one. Instance types:
                           c1.medium|c1.xlarge|c3.4xlarge|c3.8xlarge|cc1.4xlarge|cc2.8xlarge|cg1.4xlarge|cr1.8xlarge|g2.2xlarge|m1.large|m1.medium|m1.small|m1.xlarge|m2.2xlarge|m2.4xlarge|m2.xlarge|m3.2xlarge|m3.xlarge|t1.micro
  -a,--availability-zone AVAILABILITY_ZONE
                           Availability zone. Can be repated. E.g. us-east-1a
  -p,--product-description PRODUCT_DESCRIPTION
                           EC2 product description string. Options:
                           Linux/UNIX|Linux/UNIX (Amazon VPC)|SUSE Linux|SUSE
                           Linux (Amazon VPC)|Windows|Windows (Amazon
                           VPC) (default: Linux/UNIX)
  -s,--sigmas SIGMAS       Stability is measured in number of times deviated >
                           SIGMAS sigmas from the most frequent
                           value. (default: 3)
```

Instance pairs are a pair of the instance types and how many you would
use. The idea is you give it several instance types and set their
quantities so that they are roughly equivalent (i.e. 10 tiny
instances, 1 extra large) so that you are doing an apples-to-apples comparison.

Output:

```
spot-instance-utility m1.xlarge,2 m3.xlarge -d 1w
"AvailabilityZone"      "AverageCost"   "InstanceType"  "TimesDeviated"
"us-east-1a"    "3.21e-2"       "m3.xlarge"     "0"
"us-east-1e"    "6.4e-2"        "m1.xlarge"     "0"
"us-east-1a"    "8.912762278978355e-2"  "m1.xlarge"     "2"
"us-east-1b"    "0.10720401225114828"   "m1.xlarge"     "43"
"us-east-1c"    "0.2724191843393155"    "m1.xlarge"     "52"
"us-east-1b"    "0.4501"        "m3.xlarge"     "0"
"us-east-1e"    "0.4501"        "m3.xlarge"     "0"
"us-east-1c"    "1.2923384615384612"    "m3.xlarge"     "0"
```

## Status

Currently a work in process. Some TODOs:

1. Better testing on analysis.
2. Better heuristics on ordering. Currently sorts by price ascending
   and then stability descending. This is not as robust ast it could
   be
3. Use `ec2-describe-availability-zones` so that we can take a
   *region* and compare all zones within that region.
4. Advisor capability.

using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Vertical.Utils.Enums
{
    public static class EnumerableExt
    {
        public static BigInteger AccumulateEnums(this IEnumerable<BigInteger> enums) => enums.Aggregate((a, b) => a | b);
    }
}

using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public static class EnumBaseExt
    {
        private static readonly string myToReplace = string.Join("", nameof(GrammarAttributes), ".");
        private static Dictionary<BigInteger, string> myBuffer = new Dictionary<BigInteger, string>();

        public static string GetGrammarId(this BigInteger attributes)
        {
            using var _t = Trace.Entering();


            if (!myBuffer.TryGetValue(attributes, out var result))
            {
                result = GrammarAttributes.Instance.GetFullName(attributes).Substring(myToReplace.Length);
                myBuffer[attributes] = result;
            }

            return result;
        }

        public static string GetGrammarId(this EnumBase source) => ((BigInteger)source).GetGrammarId();
    }
}

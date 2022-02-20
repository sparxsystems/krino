using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public static class EnumBaseExt
    {
        private static readonly string myToReplace = string.Join("", nameof(GrammarAttributes), ".");

        public static string GetGrammarId(this BigInteger attributes)
        {
            var result = GrammarAttributes.Instance.GetFullName(attributes).Replace(myToReplace, "");
            return result;
        }

        public static string GetGrammarId(this EnumBase source) => ((BigInteger)source).GetGrammarId();
    }
}

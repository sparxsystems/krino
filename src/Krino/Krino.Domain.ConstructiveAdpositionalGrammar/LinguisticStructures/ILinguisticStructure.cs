using System.Numerics;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ILinguisticStructure
    {
        BigInteger Attributes { get; }

        string AttributesStr { get; }

        string Value { get; }

        string GrammarStr { get; }

        void BuildFormattedGrammarStr(int indent, StringBuilder builder);

        ILinguisticStructure DeepCopy();
    }
}

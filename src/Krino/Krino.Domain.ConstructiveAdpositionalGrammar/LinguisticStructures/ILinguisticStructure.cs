using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ILinguisticStructure
    {
        IAdTree AdTree { get; }

        BigInteger Attributes { get; }

        string Value { get; }
    }
}

using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ILinguisticStructureFactory
    {
        IWord CreateWord(IAdTree wordAdTree, BigInteger attributes);
        ITerm CreateTerm(IAdTree termAdTree, BigInteger attributes);
        IClause CreateClause(IAdTree clauseAdTree, BigInteger attributes);
        ISentence CreateSentence(IAdTree sentenceAdTree);
    }
}

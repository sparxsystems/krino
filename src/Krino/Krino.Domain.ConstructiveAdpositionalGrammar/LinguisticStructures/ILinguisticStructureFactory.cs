using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ILinguisticStructureFactory
    {
        IWord CreateWord(IAdTree wordAdTree);
        ITerm CreateTerm(IAdTree termAdTree, BigInteger attributes);
        IClause CreateClause(IAdTree clauseAdTree);
        ISentence CreateSentence(IAdTree sentenceAdTree);
    }
}

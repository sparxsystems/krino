using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Word : IWord
    {
        public Word(IAdTree wordAdTree)
        {
            AdTree = wordAdTree;
        }

        public IAdTree AdTree { get; private set; }

        public BigInteger StructureAttributes => 0;

        public string Value => AdTree.Phrase;
    }
}

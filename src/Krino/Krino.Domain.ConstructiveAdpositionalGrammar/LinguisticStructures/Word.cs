using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class Word : LinguisticStructureBase, IWord
    {
        private IAdTree myWord;

        public Word(IAdTree word)
            : base(word.Morpheme.Attributes)
        {
            myWord = word;
        }

        public string Value => string.Join("", Morphemes.Select(x => x.Morph));

        public IEnumerable<Morpheme> Morphemes => myWord.Select(x => x.Morpheme);
    }
}

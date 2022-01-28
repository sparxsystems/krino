using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class Word : LinguisticStructureBase, IWord
    {
        private IAdTree myWord;

        public Word(EnumRootBase enumRoot, IAdTree word)
            : base(enumRoot, word.Morpheme.Attributes)
        {
            myWord = word;
        }

        public string Value => string.Join("", Morphemes.Select(x => x.Morph));

        public IEnumerable<Morpheme> Morphemes => myWord.Select(x => x.Morpheme);
    }
}

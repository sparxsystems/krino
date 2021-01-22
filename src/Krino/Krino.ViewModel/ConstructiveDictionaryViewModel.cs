using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using System.Collections.Generic;

namespace Krino.ViewModel
{
    public class ConstructiveDictionaryViewModel : IConstructiveDictionaryViewModel
    {
        private IConstructiveDictionary myConstructiveDictionary = new ConstructiveDictionary(DictionaryContent.AttributesModel, DictionaryContent.Morphemes, DictionaryContent.Patterns);

        public IEnumerable<Morpheme> Lexemes => myConstructiveDictionary.Lexemes;

        public IEnumerable<Morpheme> NonLexemes => myConstructiveDictionary.NonLexemes;

        public IEnumerable<Pattern> Patterns => myConstructiveDictionary.Patterns;
    }
}

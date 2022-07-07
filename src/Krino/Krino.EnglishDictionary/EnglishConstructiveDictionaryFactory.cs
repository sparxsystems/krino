using Krino.ConstructiveGrammar.Dictionary;

namespace Krino.EnglishDictionary
{
    public class EnglishConstructiveDictionaryFactory
    {
        public IConstructiveDictionary Create()
        {
            var result = new ConstructiveDictionary(MorphemeProvider.Morphemes);
            return result;
        }
    }
}

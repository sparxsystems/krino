using Krino.ConstructiveGrammar.Dictionary;
using Krino.EnglishGrammar.Morphology;
using Krino.EnglishGrammar.Syntax;
using Krino.Vertical.Utils.Diagnostic;

namespace Krino.EnglishDictionary
{
    public class EnglishConstructiveDictionaryFactory
    {
        public IConstructiveDictionary Create()
        {
            _ = Trace.Entering();

            var syntax = new EnglishMachine(true);
            var morphology = new EnglishMorphology();
            var result = new ConstructiveDictionary(morphology, syntax.Machine, MorphemeProvider.Morphemes);
            return result;
        }
    }
}

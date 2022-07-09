using Krino.ConstructiveGrammar.Dictionary;
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
            var result = new ConstructiveDictionary(MorphemeProvider.Morphemes, syntax.Machine);
            return result;
        }
    }
}

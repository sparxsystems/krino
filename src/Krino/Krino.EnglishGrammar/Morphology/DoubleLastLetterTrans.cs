using Krino.Vertical.Utils.Transformations;
using System.Linq;

namespace Krino.EnglishGrammar.Morphology
{
    public class DoubleLastLetterTrans : ITransformation<string>
    {
        public string Transform(string value) => string.Concat(value, value?.LastOrDefault());

        public bool Equals(ITransformation<string> other) => other is DoubleLastLetterTrans;
    }
}

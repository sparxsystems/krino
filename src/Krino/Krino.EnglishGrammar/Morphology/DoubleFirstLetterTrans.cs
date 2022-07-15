using Krino.Vertical.Utils.Transformations;
using System.Linq;

namespace Krino.EnglishGrammar.Morphology
{
    public class DoubleFirstLetterTrans : ITransformation<string>
    {
        public string Transform(string value) => string.Concat(value?.FirstOrDefault(), value);

        public bool Equals(ITransformation<string> other) => other is DoubleLastLetterTrans;
    }
}

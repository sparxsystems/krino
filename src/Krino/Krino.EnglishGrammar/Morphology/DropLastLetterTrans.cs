using Krino.Vertical.Utils.Transformations;

namespace Krino.EnglishGrammar.Morphology
{
    public class DropLastLetterTrans : ITransformation<string>
    {
        public string Transform(string value) => value?.Substring(0, value.Length - 1);

        public bool Equals(ITransformation<string> other) => other is DropLastLetterTrans;
    }
}

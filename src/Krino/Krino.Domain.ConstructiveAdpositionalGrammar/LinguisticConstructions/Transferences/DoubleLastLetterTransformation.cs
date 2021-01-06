using Krino.Vertical.Utils.Transformations;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Transferences
{
    /// <summary>
    /// Doubles the last letter in the word.
    /// </summary>
    public class DoubleLastLetterTransformation : ITransformation<string>
    {
        public string Transform(string value)
        {
            char lastLetter = value[value.Length - 1];
            string result = value + lastLetter;
            return result;
        }

        public bool Equals(ITransformation<string> other) => other is DoubleLastLetterTransformation;
    }
}

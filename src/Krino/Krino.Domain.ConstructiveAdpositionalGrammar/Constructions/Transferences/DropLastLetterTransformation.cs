using Krino.Vertical.Utils.Transformations;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Transferences
{
    /// <summary>
    /// Drops the last letter in the word.
    /// </summary>
    public class DropLastLetterTransformation : ITransformation<string>
    {
        public string Transform(string value)
        {
            string result = value;

            if (!string.IsNullOrEmpty(value))
            {
                result = value.Substring(0, value.Length - 1);
            }

            return result;
        }


        public bool Equals(ITransformation<string> other) => other is DropLastLetterTransformation;
    }
}

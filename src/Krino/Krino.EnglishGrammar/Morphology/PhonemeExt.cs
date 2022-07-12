using System.Collections.Generic;
using System.Linq;

namespace Krino.EnglishGrammar.Morphology
{
    public static class PhonemeExt
    {
        public static IReadOnlyList<Phoneme> GetPhonemes(this string s) => s.Select(x => x.GetPhoneme()).ToList();

        public static Phoneme GetPhoneme(this char c)
        {
            switch (char.ToLower(c))
            {
                case 'b':
                case 'c':
                case 'd':
                case 'f':
                case 'g':
                case 'h':
                case 'j':
                case 'k':
                case 'l':
                case 'm':
                case 'n':
                case 'p':
                case 'q':
                case 'r':
                case 's':
                case 't':
                case 'v':
                case 'x':
                case 'z': return Phoneme.Consonant;

                case 'a':
                case 'e':
                case 'i':
                case 'o':
                case 'u': return Phoneme.Vowel;

                case 'y':
                case 'w': return Phoneme.SemiVowel;
            }

            return Phoneme.None;
        }
    }
}


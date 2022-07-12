namespace Krino.EnglishGrammar.Morphology
{
    public static class EnglishWordTrans
    {
        public static DoubleLastLetterTrans DoubleLastLetter() => new DoubleLastLetterTrans();

        public static DropLastLetterTrans DropLastLetter() => new DropLastLetterTrans();
    }
}

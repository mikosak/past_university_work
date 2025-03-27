from django import forms
from what_the_book.models import BookToRequest, Review
from what_the_book.models import User as UserPicture
from django.contrib.auth.models import User


class BookRequestForm(forms.ModelForm):
    title = forms.CharField(max_length=128,
                            help_text="Please enter the title of the Book you would like to add")
    author = forms.CharField(max_length=128,
                             help_text="Please enter the author of the Book you would like to add")

    class Meta:
        model = BookToRequest
        fields = ('title', 'author')


class ReviewForm(forms.ModelForm):
    title = forms.CharField(max_length=128,
                            help_text="Please enter title for review")
    mainText = forms.CharField(max_length=4000,
                               help_text="Please write your review")

    class Meta:
        model = Review
        fields = ('title', 'mainText')


class UserForm(forms.ModelForm):
    password = forms.CharField(widget=forms.PasswordInput())

    class Meta:
        model = User
        fields = ('username', 'password',)


class UserPictureForm(forms.ModelForm):
    class Meta:
        model = UserPicture
        fields = ('profilePicture',)

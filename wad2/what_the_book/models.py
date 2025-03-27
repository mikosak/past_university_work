from django.db import models
from django.template.defaultfilters import slugify
from datetime import datetime


class User(models.Model):
    userId = models.AutoField(unique=True, primary_key=True)
    username = models.CharField(max_length=20)
    password = models.CharField(max_length=20)
    profilePicture = models.ImageField(blank=True)

    def save(self, *args, **kwargs):
        self.slug = slugify(self.username)
        super(User, self).save(*args, **kwargs)

    class Meta:
        verbose_name_plural = 'users'

    def __str__(self):
        return f"{self.username}"


class Admin(models.Model):
    userId = models.AutoField(unique=True, primary_key=True)
    username = models.CharField(max_length=20)
    password = models.CharField(max_length=20)
    profilePicture = models.ImageField()

    def __str__(self):
        return f"{self.username}"


class Book(models.Model):
    addedBy = models.ForeignKey(
        Admin, on_delete=models.CASCADE, null=True, blank=True)
    title = models.CharField(max_length=30)
    author = models.CharField(max_length=20)
    coverPicture = models.ImageField(
        upload_to='images/book_covers/', blank=True)
    slug = models.SlugField(unique=True)

    def save(self, *args, **kwargs):
        self.slug = slugify(self.title)
        super(Book, self).save(*args, **kwargs)

    class Meta:
        verbose_name_plural = 'books'

    def __str__(self):
        return f"{self.title} by {self.author}"


class BookToRequest(models.Model):
    requestedBy = models.ForeignKey(
        User, on_delete=models.CASCADE, null=True, blank=True)
    readBy = models.ForeignKey(
        Admin, on_delete=models.CASCADE, null=True, blank=True)
    title = models.CharField(max_length=30)
    author = models.CharField(max_length=20)

    def save(self, *args, **kwargs):
        self.slug = slugify(self.title)
        super(BookToRequest, self).save(*args, **kwargs)

    class Meta:
        verbose_name_plural = 'requested books'

    def __str__(self):
        return f"{self.title} requested by {self.requestedBy}"


class Review(models.Model):
    reviewOf = models.ForeignKey(
        Book, on_delete=models.CASCADE, null=True, blank=True)
    createdBy = models.ForeignKey(
        User, on_delete=models.CASCADE, null=True, blank=True)
    title = models.CharField(max_length=30)
    mainText = models.TextField()
    createdOn = models.DateField()
    likes = models.IntegerField(default=0)

    def save(self, *args, **kwargs):
        if self.likes is not None:
            if self.likes < 0:
                self.likes = 0
        if self.createdOn is not None:
            if self.createdOn > datetime.now().date():
                self.createdOn = datetime.now().date()
        self.slug = slugify(self.title)
        super(Review, self).save(*args, **kwargs)

    class Meta:
        verbose_name_plural = 'reviews'

    def __str__(self):
        return f"Review of {self.reviewOf}  by {self.createdBy}"
